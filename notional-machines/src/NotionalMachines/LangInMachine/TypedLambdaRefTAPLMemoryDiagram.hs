{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module NotionalMachines.LangInMachine.TypedLambdaRefTAPLMemoryDiagram where

import Control.Monad.State.Lazy (StateT)
import Control.Monad ((<=<))

import qualified Data.Map as Map

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Location,
                                                            Term (..), StateRacket (StateRacket))
import NotionalMachines.Machine.TAPLMemoryDiagram.Main     (TAPLMemoryDiagram (..), DTerm (..), DLocation (DLoc))
import NotionalMachines.Meta.Bisimulation                  (Bisimulation (..))
import NotionalMachines.Meta.Steppable (stepM)
import NotionalMachines.Meta.Injective (Injective (..))
import NotionalMachines.Utils (eitherToMaybe, stateToTuple, mapMapM)

pattern MDVar name          = Leaf name
pattern MDLambda name typ t = Branch [Leaf "(\\", Leaf name, Leaf ":", Leaf typ, Leaf ".", t, Leaf ")"]
pattern MDApp t1 t2         = Branch [t1, t2]
-- Unit
pattern MDUnit              = Leaf "unit"
-- Sequence
pattern MDSeq t1 t2         = Branch [t1, Leaf ";", t2]
-- References
pattern MDRef t             = Branch [Leaf "ref", Leaf " ", t]
pattern MDDeref t           = Branch [Leaf "!", t]
pattern MDAssign t1 t2      = Branch [t1, Leaf ":=", t2]
-- Booleans
pattern MDTru               = Leaf "true"
pattern MDFls               = Leaf "false"
pattern MDIf t1 t2 t3       = Branch [Leaf "if", Leaf " ", t1, Leaf " ", t2, Leaf " ", t3]
-- Arithmetic Expressions
pattern MDZero              = Leaf "0"
pattern MDSucc t            = Branch [Leaf "succ",   Leaf " ", t]
pattern MDPred t            = Branch [Leaf "pred",   Leaf " ", t]
pattern MDIsZero t          = Branch [Leaf "iszero", Leaf " ", t]

termToDTerm :: Term -> DTerm Location
termToDTerm = \case
  -- Lambdas
  Var name          -> MDVar name
  Lambda name typ t -> MDLambda name (show typ) (rec t)
  Closure {}        -> error "Ala Wadler not covered"
  App t1 t2         -> MDApp (rec t1) (rec t2)
  -- Unit
  Unit              -> MDUnit
  -- Sequence
  Seq t1 t2         -> MDSeq (rec t1) (rec t2)
  -- References
  Ref t             -> MDRef (rec t)
  Deref t           -> MDDeref (rec t)
  Assign t1 t2      -> MDAssign (rec t1) (rec t2)
  Loc l             -> TLoc (DLoc l)
  -- Booleans
  Tru               -> MDTru
  Fls               -> MDFls
  If t1 t2 t3       -> MDIf (rec t1) (rec t2) (rec t3)
  -- Arithmetic Exprsions
  Zero              -> MDZero
  Succ t            -> MDSucc (rec t)
  Pred t            -> MDPred (rec t)
  IsZero t          -> MDIsZero (rec t)
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
  -- Booleans
  MDTru               -> return Tru
  MDFls               -> return Fls
  MDIf t1 t2 t3       -> If <$> rec t1 <*> rec t2 <*> rec t3
  -- Arithmetic Expressions
  MDZero              -> return Zero
  MDSucc   t          -> Succ <$> rec t
  MDPred   t          -> Pred <$> rec t
  MDIsZero t          -> IsZero <$> rec t
  -- Lambdas
  MDLambda name typ t -> Lambda name (read typ) <$> rec t
  MDApp t1 t2         -> App <$> rec t1 <*> rec t2
  MDVar name          -> return $ Var name
  _ -> Nothing
  where rec = dTermToTerm

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
        step = eitherToMaybe . stateToTuple (stepM :: Term -> StateT StateRacket (Either Error) Term)
