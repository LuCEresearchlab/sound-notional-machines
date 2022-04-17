{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module NotionalMachines.LangInMachine.TypedLambdaRefTAPLMemoryDiagram where

import Control.Monad.State.Lazy (StateT)
import Control.Monad ((<=<))

import Data.List (intersperse)

import qualified Data.Map as Map

import Prettyprinter (Pretty (pretty))

import Diagrams.Prelude (Diagram, vsep, bgFrame, white, hrule, width)
import Diagrams.Backend.Rasterific.CmdLine (B)

import NotionalMachines.Lang.TypedLambdaRef.Main (langPipeline, MachineStateAlaRacket(..), Trace(..), traceAlaRacket)
import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Location,
                                                            Term (..), StateRacket (StateRacket), isNumVal, peanoToDec, typecheck)
import NotionalMachines.Machine.TAPLMemoryDiagram.Main     (TAPLMemoryDiagram (..), DTerm (..), DLocation (DLoc))
import NotionalMachines.Machine.TAPLMemoryDiagram.Diagram  (toDiagram)

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Steppable (stepM)
import NotionalMachines.Meta.Injective (Injective (..))

import NotionalMachines.Utils (eitherToMaybe, stateToTuple, mapMapM, mkCmd, mkLangReplOpts, renderD, prettyToString)
import Text.Read (readMaybe)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parseType)

pattern MDVar name          = Branch [Leaf name]
pattern MDLambda name typ t = Branch [Leaf "(\\", Leaf name, Leaf " : ", Leaf typ, Leaf ". ", t, Leaf ")"]
pattern MDApp t1 t2         = Branch [t1, Leaf " ", t2]
-- Unit
pattern MDUnit              = Branch [Leaf "unit"]
-- Sequence
pattern MDSeq t1 t2         = Branch [t1, Leaf "; ", t2]
-- References
pattern MDRef t             = Branch [Leaf "ref", Leaf " ", t]
pattern MDDeref t           = Branch [Leaf "!", t]
pattern MDAssign t1 t2      = Branch [t1, Leaf " := ", t2]
-- Compound data
pattern MDTuple ts         <- Branch (tupleElems -> Just ts) where
        MDTuple ts          = Branch $ [Leaf "{"] ++ intersperse (Leaf ",") ts ++ [Leaf "}"]
pattern MDProj i t          = Branch [t, Leaf ".", Leaf i]
-- Booleans
pattern MDTru               = Branch [Leaf "true"]
pattern MDFls               = Branch [Leaf "false"]
pattern MDIf t1 t2 t3       = Branch [Leaf "if", Leaf " ", t1, Leaf " ", t2, Leaf " ", t3]
-- Arithmetic Expressions
pattern MDZero              = Branch [Leaf "0"]
pattern MDSucc t            = Branch [Leaf "succ",   Leaf " ", t]
pattern MDNat n             = Branch [Leaf "$nat",   Leaf " ", Leaf n]
pattern MDPred t            = Branch [Leaf "pred",   Leaf " ", t]
pattern MDIsZero t          = Branch [Leaf "iszero", Leaf " ", t]

tupleElems :: Eq l => [DTerm l] -> Maybe [DTerm l]
tupleElems = \case
  []                   -> Nothing
  [Leaf "{", Leaf "}"] -> Just []
  [Leaf "{", _]        -> Nothing
  ((Leaf "{"):xs) | last xs == Leaf "}" -> commaSep (init xs)
  _                    -> Nothing
  where commaSep = \case
          []              -> Just []
          [Leaf ","]      -> Nothing
          [x]             -> Just [x]
          [_, _]          -> Nothing
          (x:Leaf ",":xs) -> (x :) <$> commaSep xs
          _               -> Nothing

termToDTerm :: Term -> DTerm Location
termToDTerm = \case
  -- Lambdas
  Var name            -> MDVar name
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
  Succ t | isNumVal t -> (MDNat . show . peanoToDec . Succ) t
  Succ t | otherwise  -> MDSucc (rec t)
  Pred t              -> MDPred (rec t)
  IsZero t            -> MDIsZero (rec t)
  where rec = termToDTerm

dTermToTerm :: DTerm Location -> Maybe Term
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
  MDProj i t          -> Proj <$> readMaybe i <*> rec t
  -- Booleans
  MDTru               -> return Tru
  MDFls               -> return Fls
  MDIf t1 t2 t3       -> If <$> rec t1 <*> rec t2 <*> rec t3
  -- Arithmetic Expressions
  MDZero              -> return Zero
  MDNat n             -> decToPeano =<< readMaybe n
  MDSucc t            -> Succ <$> rec t
  MDPred t            -> Pred <$> rec t
  MDIsZero t          -> IsZero <$> rec t
  -- Lambdas
  MDLambda name typ t -> Lambda name <$> eitherToMaybe (parseType typ) <*> rec t
  MDApp t1 t2         -> App <$> rec t1 <*> rec t2
  MDVar name          -> return $ Var name
  _ -> Nothing
  where rec = dTermToTerm

        decToPeano :: Integer -> Maybe Term
        decToPeano 0         = Just Zero
        decToPeano n | n > 0 = Succ <$> decToPeano (n - 1)
        decToPeano _         = Nothing -- error $ "internal error: negative numbers are not supported: " ++ show n

langToNM :: (Term, StateRacket) -> TAPLMemoryDiagram Location
langToNM (term, StateRacket env store) = TAPLMemoryDiagram (termToDTerm term)
                                                           (Map.map termToDTerm env)
                                                           (Map.map termToDTerm (Map.mapKeys DLoc store))

nmToLang :: TAPLMemoryDiagram Location -> Maybe (Term, StateRacket)
nmToLang (TAPLMemoryDiagram dTerm dEnv dStore) =
  do term <- dTermToTerm dTerm
     env <- mapMapM dTermToTerm dEnv
     store <- mapMapM dTermToTerm (Map.mapKeys (\(DLoc l) -> l) dStore)
     return (term, StateRacket env store)

instance Injective (Term, StateRacket) (TAPLMemoryDiagram Location) where
  toNM   = langToNM
  fromNM = nmToLang


bisim :: Bisimulation (Term, StateRacket)
                      (Maybe (Term, StateRacket))
                      (TAPLMemoryDiagram Location)
                      (Maybe (TAPLMemoryDiagram Location))
bisim = MkBisim { fLang  = step
                , fNM    = fmap toNM . step <=< fromNM
                , alphaA = toNM
                , alphaB = fmap toNM }
  where step :: (Term, StateRacket) -> Maybe (Term, StateRacket)
        step x = eitherToMaybe $ stateToTuple s x <* typecheck (fst x)
        s = stepM :: Term -> StateT StateRacket (Either Error) Term


-------------------------
-- REPL -----------------
-------------------------

-- | Start a REPL for the TAPL mem diagram notional machine. The svg output goes
-- to a file given as argument scaled to be rendered with @w@ pixels.
-- TODO: use w. sizes should be relative
-- TODO: allow for different displays of diaSeq
-- TODO: arrow heads should be smaller
repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("traceNameEnv", mkCmd . traceAlaRacket)
    , ("renderTrace", either (print . pretty) (render . traceDiagram) . traceAlaRacket) ]
    "TAPLMemoryDiagram>" helpMsg langPipeline
  where helpMsg = "Play with the TAPL Memory Diagram notional machine for Lambda Calculus with References"
        traceDiagram :: Trace MachineStateAlaRacket -> Diagram B
        traceDiagram (Trace ss) = vsep 1 $ intersperse (hrule (maxWidth dias)) dias
          where dias = map (\(MachineStateAlaRacket s) -> toDiagram 10 (langToNM s)) ss
                maxWidth = maximum . map width
        render :: Diagram B -> IO ()
        render = renderD fileName w . bgFrame 0.05 white
