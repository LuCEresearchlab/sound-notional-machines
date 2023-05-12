{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

module NotionalMachines.LangInMachine.UntypedArithExpressionTutor where

import Control.Monad.State.Lazy (State, StateT (..), lift, liftM3)

import Data.Set (Set)

import NotionalMachines.Lang.UntypedArith.Main       (Term (..))
import NotionalMachines.Machine.ExpressionTutor.Main

import NotionalMachines.Meta.Bisimulation (Bisimulation, mkInjBisim)
import NotionalMachines.Meta.Injective    (Injective (..))
import NotionalMachines.Meta.LangToNM     (LangToNM (..))
import NotionalMachines.Meta.Steppable    (Steppable (..))

pattern NodeTrue   :: Int -> Node
pattern NodeFalse  :: Int -> Node
pattern NodeIf     :: Int -> Node
pattern NodeZero   :: Int -> Node
pattern NodeSucc   :: Int -> Node
pattern NodePred   :: Int -> Node
pattern NodeIsZero :: Int -> Node

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

arithToET :: Term -> ExpTutorDiagram
arithToET = langToET go
  where go :: Term -> State Int ExpTutorDiagram
        go = \case
          Tru         -> newDiaLeaf   NodeTrue
          Fls         -> newDiaLeaf   NodeFalse
          If t1 t2 t3 -> newDiaBranch NodeIf     go [t1, t2, t3]
          Zero        -> newDiaLeaf   NodeZero
          Succ t      -> newDiaBranch NodeSucc   go [t]
          Pred t      -> newDiaBranch NodePred   go [t]
          IsZero t    -> newDiaBranch NodeIsZero go [t]

etToArith :: ExpTutorDiagram -> Maybe Term
etToArith = etToLang go
  where
    go :: ExpTutorDiagram -> StateT (Set Int) Maybe Term
    go d = checkCycle d $ case d of
      DiaLeaf   NodeTrue {}            -> return Tru
      DiaLeaf   NodeFalse {}           -> return Fls
      DiaBranch NodeIf {} ts@[_, _, _] -> let [n1, n2, n3] = fmap go ts
                                           in liftM3 If n1 n2 n3
      DiaLeaf   NodeZero {}            -> return Zero
      DiaBranch NodeSucc {}   [t]      -> Succ   <$> go t
      DiaBranch NodePred {}   [t]      -> Pred   <$> go t
      DiaBranch NodeIsZero {} [t]      -> IsZero <$> go t
      _ -> lift Nothing -- "incorrect diagram"

instance LangToNM Term ExpTutorDiagram where
  toNM   = arithToET

instance Injective Term ExpTutorDiagram Maybe where
  fromNM = etToArith

bisim :: Bisimulation Term Term ExpTutorDiagram (Maybe ExpTutorDiagram)
bisim = mkInjBisim step
