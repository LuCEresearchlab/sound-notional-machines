{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures -Wno-orphans #-}

{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses, LambdaCase #-}

module NotionalMachines.LangInMachine.UntypedArithExpressionTutor where

import Control.Monad.State.Lazy (State, StateT(..), liftM3, lift)

import Data.Set (Set)

import NotionalMachines.Lang.UntypedArith.Main (Term(..))
import NotionalMachines.Machine.ExpressionTutor.Main

import NotionalMachines.Meta.Injective
import NotionalMachines.Meta.Bisimulation
import NotionalMachines.Meta.Steppable

pattern NodeTrue   i =  MkNode i Nothing [C "true"]
pattern NodeFalse  i =  MkNode i Nothing [C "false"]
pattern NodeIf     i <- MkNode i Nothing [Hole {}, Hole {}, Hole {}] where
        NodeIf     i =  MkNode i Nothing [holeP,   holeP,   holeP]
pattern NodeZero   i =  MkNode i Nothing [C "0"]
pattern NodeSucc   i <- MkNode i Nothing [C "succ",   Hole {}] where
        NodeSucc   i =  MkNode i Nothing [C "succ",   holeP]
pattern NodePred   i <- MkNode i Nothing [C "pred",   Hole {}] where
        NodePred   i =  MkNode i Nothing [C "pred",   holeP]
pattern NodeIsZero i <- MkNode i Nothing [C "iszero", Hole {}] where
        NodeIsZero i =  MkNode i Nothing [C "iszero", holeP]

arithToET :: Term -> ExpTreeDiagram
arithToET = langToET go
  where go :: Term -> State Int ExpTreeDiagram
        go = \case
          Tru         -> newDiaLeaf   NodeTrue
          Fls         -> newDiaLeaf   NodeFalse
          If t1 t2 t3 -> newDiaBranch NodeIf     go [t1, t2, t3]
          Zero        -> newDiaLeaf   NodeZero
          Succ t      -> newDiaBranch NodeSucc   go [t]
          Pred t      -> newDiaBranch NodePred   go [t]
          IsZero t    -> newDiaBranch NodeIsZero go [t]

etToArith :: ExpTreeDiagram -> Maybe Term
etToArith = etToLang go
  where
    go :: ExpTreeDiagram -> StateT (Set Int) Maybe Term
    go d = checkCycle d $ case d of
      DiaLeaf   NodeTrue {}              -> return Tru
      DiaLeaf   NodeFalse {}             -> return Fls
      DiaBranch NodeIf {} ts @ [_, _, _] -> let [n1, n2, n3] = fmap go ts
                                                  in liftM3 If n1 n2 n3
      DiaLeaf   NodeZero {}              -> return Zero
      DiaBranch NodeSucc {}   [t]        -> Succ   <$> go t
      DiaBranch NodePred {}   [t]        -> Pred   <$> go t
      DiaBranch NodeIsZero {} [t]        -> IsZero <$> go t
      _ -> lift Nothing -- "incorrect diagram"

instance Injective Term ExpTreeDiagram where
  toNM   = arithToET
  fromNM = etToArith

bisim :: Bisimulation Term Term ExpTreeDiagram (Maybe ExpTreeDiagram)
bisim = mkInjBisim step