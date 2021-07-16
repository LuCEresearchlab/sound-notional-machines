{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures -Wno-orphans #-}

{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses, LambdaCase #-}

module NotionalMachines.Lang.ArithExpressionTutor where

import Control.Monad.State.Lazy (State, StateT(..), liftM3)

import Data.Set (Set)

import NotionalMachines.Lang.Arith (Term(..))
import NotionalMachines.Machine.ExpressionTutor

import NotionalMachines.Meta.Injective
import NotionalMachines.Meta.Bisimulation
import NotionalMachines.Meta.Steppable

pattern NodeTrue   i =  MkNode i [C "true"]
pattern NodeFalse  i =  MkNode i [C "false"]
pattern NodeIf     i <- MkNode i [Hole _, Hole _, Hole _] where
        NodeIf     i =  MkNode i [holeP,  holeP,  holeP]
pattern NodeZero   i =  MkNode i [C "0"]
pattern NodeSucc   i <- MkNode i [C "succ", Hole _] where
        NodeSucc   i =  MkNode i [C "succ", holeP]
pattern NodePred   i <- MkNode i [C "pred", Hole _] where
        NodePred   i =  MkNode i [C "pred", holeP]
pattern NodeIsZero i <- MkNode i [C "iszero", Hole _] where
        NodeIsZero i =  MkNode i [C "iszero", holeP]

arithToET :: Term -> ExpTreeDiagram
arithToET = langToET go
  where go :: Term -> State Int ExpTreeDiagram
        go = \case
          Tru         -> newDiaLeaf   NodeTrue
          Fls         -> newDiaLeaf   NodeFalse
          If t1 t2 t3 -> newDiaBranch NodeIf go [t1, t2, t3]
          Zero        -> newDiaLeaf   NodeZero
          Succ t      -> newDiaBranch NodeSucc       go [t]
          Pred t      -> newDiaBranch NodePred       go [t]
          IsZero t    -> newDiaBranch NodeIsZero     go [t]

etToArith :: ExpTreeDiagram -> Maybe Term
etToArith = etToLang go
  where
    go :: ExpTreeDiagram -> StateT (Set Int) Maybe Term
    go = \case
      DiaLeaf   (NodeTrue   i)                -> checkCycle i (return Tru)
      DiaLeaf   (NodeFalse  i)                -> checkCycle i (return Fls)
      DiaBranch (NodeIf     i) ts @ [_, _, _] -> let [n1, n2, n3] = fmap go ts
                                                 in checkCycle i (liftM3 If n1 n2 n3)
      DiaLeaf   (NodeZero   i)                -> checkCycle i (return Zero)
      DiaBranch (NodeSucc   i) [t]            -> checkCycle i (Succ   <$> go t)
      DiaBranch (NodePred   i) [t]            -> checkCycle i (Pred   <$> go t)
      DiaBranch (NodeIsZero i) [t]            -> checkCycle i (IsZero <$> go t)
      _ -> StateT (const Nothing) -- "incorrect diagram"

instance Injective Term ExpTreeDiagram where
  toNM   = arithToET
  fromNM = etToArith

bisim :: Bisimulation Term Term ExpTreeDiagram (Maybe ExpTreeDiagram)
bisim = mkInjBisim step
