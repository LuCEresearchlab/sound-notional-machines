{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module NotionalMachines.LangInMachine.TypedLambdaRefTAPLMemoryDiagram (
    bisim
  , nmToLang
  , langToNM
  , repl
  , diagramTrace
  ) where

import Control.Monad            ((<=<))
import Control.Monad.State.Lazy (StateT)

import           Data.Char (isNumber)
import           Data.List (intersperse)
import qualified Data.Map  as Map

import Text.Read (readMaybe)

import Diagrams.Backend.Rasterific (renderRasterific)
import Diagrams.Prelude            (Diagram)

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error (..), Location,
                                                            StateRacket (StateRacket), Term (..),
                                                            emptyStateAlaRacket, isNumVal,
                                                            peanoToDec, typecheck)
import NotionalMachines.Lang.TypedLambdaRef.Main           (langPipeline, parse, traceAlaRacket)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parseType)

import NotionalMachines.Machine.TAPLMemoryDiagram.Main (DLocation (DLoc), DTerm (..),
                                                        TAPLMemoryDiagram (..))

import NotionalMachines.Meta.Bisimulation (Bisimulation (..), mkInjBisimM, mkStepMInjNM)
import NotionalMachines.Meta.Injective    (Injective (..))
import NotionalMachines.Meta.LangToNM     (LangToNM (..))
import NotionalMachines.Meta.Steppable    (SteppableM (traceM), stepM)

import NotionalMachines.Machine.TAPLMemoryDiagram.Diagram (TermToDiagram, termToTextDiagram,
                                                           termToTreeDiagram, toDiagramSeq)

import NotionalMachines.Util.Diagrams (renderD)
import NotionalMachines.Util.REPL     (mkCmd, mkLangReplOpts)
import NotionalMachines.Util.Util     (mapMapM, maybeToEither, prettyToString, stateToTuple)


pattern MDVarOrNum :: String    -> DTerm l
pattern MDApp      :: DTerm l   -> DTerm l -> DTerm l
pattern MDLambda   :: String    -> String  -> DTerm l -> DTerm l
-- Unit
pattern MDUnit     :: DTerm l
-- Sequence
pattern MDSeq      :: DTerm l   -> DTerm l -> DTerm l
-- References
pattern MDRef      :: DTerm l   -> DTerm l
pattern MDDeref    :: DTerm l   -> DTerm l
pattern MDAssign   :: DTerm l   -> DTerm l -> DTerm l
-- Compound data
pattern MDTuple    :: Eq l =>
                    [DTerm l] -> DTerm l
pattern MDProj     :: String    -> DTerm l -> DTerm l
-- Booleans
pattern MDTru      :: DTerm l
pattern MDFls      :: DTerm l
pattern MDIf       :: DTerm l   -> DTerm l -> DTerm l -> DTerm l
-- Arithmetic Expressions
pattern MDZero     :: DTerm l
pattern MDSucc     :: DTerm l   -> DTerm l
pattern MDPred     :: DTerm l   -> DTerm l
pattern MDIsZero   :: DTerm l   -> DTerm l

pattern MDVarOrNum s        = Branch [Leaf s]
pattern MDLambda name typ t = Branch [Leaf "(λ", Leaf name, Leaf " : ", Leaf typ, Leaf ". ", t, Leaf ")"]
pattern MDApp         t1 t2 = Branch [t1, Space, t2]
-- Unit
pattern MDUnit              = Branch [Leaf "unit"]
-- Sequence
pattern MDSeq         t1 t2 = Branch [t1, Leaf ";", Space, t2]
-- References
pattern MDRef             t = Branch [Leaf "ref", Space, t]
pattern MDDeref           t = Branch [Leaf "!", t]
pattern MDAssign      t1 t2 = Branch [t1, Space, Leaf ":=", Space, t2]
-- Compound data
pattern MDTuple         ts <- Branch (tupleElems -> Right ts) where
        MDTuple          ts = Branch $ [OTuple] ++ intersperse Comma ts ++ [CTuple]
pattern MDProj          i t = Branch [t, Leaf ".", Leaf i]
-- Booleans
pattern MDTru               = Branch [Leaf "true"]
pattern MDFls               = Branch [Leaf "false"]
pattern MDIf       t1 t2 t3 = Branch [Leaf "if",   Space, t1, Space,
                                      Leaf "then", Space, t2, Space,
                                      Leaf "else", Space, t3]
-- Arithmetic Expressions
pattern MDZero              = Branch [Leaf "0"]
pattern MDSucc            t = Branch [Leaf "succ",   Space, t]
pattern MDPred            t = Branch [Leaf "pred",   Space, t]
pattern MDIsZero          t = Branch [Leaf "iszero", Space, t]

pattern Comma, Space, OTuple, CTuple :: DTerm l
pattern Comma  = Leaf ", "
pattern Space  = Leaf " "
pattern OTuple = Leaf "{"
pattern CTuple = Leaf "}"

tupleElems :: Eq l => [DTerm l] -> Either Error [DTerm l]
tupleElems = \case
  []                              -> Left $ InternalError "Illegal NM structure"
  [OTuple, CTuple]                -> Right []
  [OTuple, _]                     -> Left $ InternalError "Illegal NM structure"
  (OTuple:xs) | last xs == CTuple -> commaSep (init xs)
  _                               -> Left $ InternalError "Illegal NM structure"
  where commaSep = \case
          []           -> Right []
          [Comma]      -> Left $ InternalError "Illegal NM structure"
          [x]          -> Right [x]
          [_, _]       -> Left $ InternalError "Illegal NM structure"
          (x:Comma:xs) -> (x :) <$> commaSep xs
          _            -> Left $ InternalError "Illegal NM structure"

termToDTerm :: Term -> DTerm Location
termToDTerm = \case
  -- Lambdas
  Var name            -> MDVarOrNum name
  Lambda name typ t   -> MDLambda name (prettyToString typ) (rec t)
  Closure {}          -> error "Ala Wadler not covered"
  App t1 t2           -> MDApp (rec t1) (rec t2)
  -- Unit
  Unit                -> MDUnit
  -- Sequence
  Seq t1 t2           -> MDSeq (rec t1) (rec t2)
  -- References
  Ref t               -> MDRef (rec t)
  Deref t             -> MDDeref (rec t)
  Assign t1 t2        -> MDAssign (rec t1) (rec t2)
  Loc l               -> TLoc (DLoc l)
  -- Compound data
  Tuple ts            -> MDTuple (map rec ts)
  Proj i t            -> MDProj (show i) (rec t)
  -- Booleans
  Tru                 -> MDTru
  Fls                 -> MDFls
  If t1 t2 t3         -> MDIf (rec t1) (rec t2) (rec t3)
  -- Arithmetic Expr  sions
  Zero                -> MDZero
  Succ t | isNumVal t -> (MDVarOrNum . show . peanoToDec . Succ) t
  Succ t | otherwise  -> MDSucc (rec t)
  Pred t              -> MDPred (rec t)
  IsZero t            -> MDIsZero (rec t)
  where rec = termToDTerm

dTermToTerm :: DTerm Location -> Either Error Term
dTermToTerm = \case
  -- Unit
  MDUnit              -> return Unit
  -- Sequence
  MDSeq t1 t2         -> Seq <$> rec t1 <*> rec t2
  -- References
  MDRef t             -> Ref <$> rec t
  MDDeref t           -> Deref <$> rec t
  MDAssign t1 t2      -> Assign <$> rec t1 <*> rec t2
  TLoc (DLoc l)       -> return $ Loc l
  -- Compound data
  MDTuple ts          -> Tuple <$> mapM rec ts
  MDProj i t          -> Proj <$> maybeToEither (InternalError "Illegal NM structure") (readMaybe i) <*> rec t
  -- Booleans
  MDTru               -> return Tru
  MDFls               -> return Fls
  MDIf t1 t2 t3       -> If <$> rec t1 <*> rec t2 <*> rec t3
  -- Arithmetic Expressions
  MDZero              -> return Zero
  MDSucc t            -> Succ <$> rec t
  MDPred t            -> Pred <$> rec t
  MDIsZero t          -> IsZero <$> rec t
  -- Lambdas
  MDLambda name typ t -> Lambda name <$> parseType typ <*> rec t
  MDApp t1 t2         -> App <$> rec t1 <*> rec t2
  MDVarOrNum s@(x:_)
        | isNumber x  -> decToPeano =<< maybeToEither (InternalError "Illegal NM structure") (readMaybe s)
        | otherwise   -> return $ Var s
  _                   -> Left $ InternalError "Illegal NM structure"
  where rec = dTermToTerm

        decToPeano :: Integer -> Either Error Term
        decToPeano 0         = Right Zero
        decToPeano n | n > 0 = Succ <$> decToPeano (n - 1)
        decToPeano n         = Left . InternalError $ "negative numbers are not supported: " ++ show n

langToNM :: (Term, StateRacket) -> TAPLMemoryDiagram Location
langToNM (term, StateRacket env store) =
  TAPLMemoryDiagram (termToDTerm term)
                    (Map.map termToDTerm env)
                    (Map.map termToDTerm (Map.mapKeys DLoc store))

nmToLang :: TAPLMemoryDiagram Location -> Either Error (Term, StateRacket)
nmToLang (TAPLMemoryDiagram dTerm dEnv dStore) =
  do term <- dTermToTerm dTerm
     env <- mapMapM dTermToTerm dEnv
     store <- mapMapM dTermToTerm (Map.mapKeys (\(DLoc l) -> l) dStore)
     return (term, StateRacket env store)

instance LangToNM (Term, StateRacket) (TAPLMemoryDiagram Location) where
  toNM   = langToNM

instance Injective (Term, StateRacket) (TAPLMemoryDiagram Location) (Either Error) where
  fromNM = nmToLang

instance SteppableM (TAPLMemoryDiagram Location) (Either Error) where
  stepM = mkStepMInjNM _stepM

bisim :: Bisimulation (Term, StateRacket)
                      (Either Error (Term, StateRacket))
                      (TAPLMemoryDiagram Location)
                      (Either Error (TAPLMemoryDiagram Location))
bisim = mkInjBisimM _stepM

_stepM :: (Term, StateRacket) -> Either Error (Term, StateRacket)
_stepM = stateToTuple s
    where s = stepM :: Term -> StateT StateRacket (Either Error) Term


-------------------------
-- REPL -----------------
-------------------------

-- | Start a REPL for the TAPL mem diagram notional machine. The svg output goes
-- to a file given as argument scaled to be rendered with @w@ pixels.
-- TODO: arrow heads should be smaller
repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("traceNameEnv", mkCmd . traceAlaRacket)
    , ("renderTreeTrace",  r . diagramTrace termToTreeDiagram)
    , ("renderTextTrace",  r . diagramTrace termToTextDiagram) ]
    "TAPLMemoryDiagram>" helpMsg langPipeline
  where helpMsg = "Play with the TAPL Memory Diagram notional machine for Lambda Calculus with References"
        r = renderD renderRasterific fileName w

diagramTrace :: _ => TermToDiagram Location b -> String -> IO (Either Error (Diagram b))
diagramTrace f = mapM (toDiagramSeq f) . (trace' <=< str2NM)

trace' :: TAPLMemoryDiagram Location -> Either Error [TAPLMemoryDiagram Location]
trace' = sequence . traceM

str2NM :: String -> Either Error (TAPLMemoryDiagram Location)
str2NM = fmap (langToNM . (, emptyStateAlaRacket) . fst) . (typecheck <=< parse)

