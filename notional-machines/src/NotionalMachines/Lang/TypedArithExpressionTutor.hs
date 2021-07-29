{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures -Wno-orphans #-}

{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses, LambdaCase #-}

module NotionalMachines.Lang.TypedArithExpressionTutor where

import Control.Monad.State.Lazy (State, StateT(..), liftM3)
import Control.Monad ((<=<))

import Data.Set (Set)
import Data.Text.Prettyprint.Doc

import           NotionalMachines.Lang.ArithExpressionTutor ()

import           NotionalMachines.Lang.Arith (Term(..))
import           NotionalMachines.Lang.TypedArith (typeof)
import qualified NotionalMachines.Lang.TypedArith as TypedArith (Type)
import           NotionalMachines.Machine.ExpressionTutor hiding (Type)
import qualified NotionalMachines.Machine.ExpressionTutor as ET (Type)

import NotionalMachines.Meta.Injective
import NotionalMachines.Meta.Bisimulation
import NotionalMachines.Meta.Steppable

newtype TyExpTreeDiagram = TyExpTreeDiagram ExpTreeDiagram deriving (Eq, Show)

pattern NodeTrue   t i =  MkNode i t [C "true"]
pattern NodeFalse  t i =  MkNode i t [C "false"]
pattern NodeIf     t i <- MkNode i t [Hole {}, Hole {}, Hole {}] where
        NodeIf     t i =  MkNode i t [holeP,   holeP,   holeP]
pattern NodeZero   t i =  MkNode i t [C "0"]
pattern NodeSucc   t i <- MkNode i t [C "succ",   Hole {}] where
        NodeSucc   t i =  MkNode i t [C "succ",   holeP]
pattern NodePred   t i <- MkNode i t [C "pred",   Hole {}] where
        NodePred   t i =  MkNode i t [C "pred",   holeP]
pattern NodeIsZero t i <- MkNode i t [C "iszero", Hole {}] where
        NodeIsZero t i =  MkNode i t [C "iszero", holeP]

arithToET :: Term -> ExpTreeDiagram
arithToET = langToET go
  where go :: Term -> State Int ExpTreeDiagram
        go s = case s of
          Tru         -> newDiaLeaf   (NodeTrue   (typeArithToET s))
          Fls         -> newDiaLeaf   (NodeFalse  (typeArithToET s))
          If t1 t2 t3 -> newDiaBranch (NodeIf     (typeArithToET s)) go [t1, t2, t3]
          Zero        -> newDiaLeaf   (NodeZero   (typeArithToET s))
          Succ t      -> newDiaBranch (NodeSucc   (typeArithToET s)) go [t]
          Pred t      -> newDiaBranch (NodePred   (typeArithToET s)) go [t]
          IsZero t    -> newDiaBranch (NodeIsZero (typeArithToET s)) go [t]

typeArithToET :: Term -> Maybe ET.Type
typeArithToET = Just . show . typeof

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
      _ -> StateT (const Nothing) -- "incorrect diagram"

instance Injective Term TyExpTreeDiagram where
  toNM = TyExpTreeDiagram . arithToET
  fromNM (TyExpTreeDiagram d) = etToArith d

-- Ask for the type of a diagram not annotated with types
typeOfBisim :: Bisimulation Term (Maybe TypedArith.Type) ExpTreeDiagram (Maybe ET.Type)
typeOfBisim = Bisim { fLang  = typeof
                    , fNM    = fmap (show . pretty) . typeof <=< fromNM
                    , alphaA = toNM
                    , alphaB = fmap (show . pretty) }

-- Annotate diagram with types
annotateTypeBisim :: Bisimulation Term Term ExpTreeDiagram (Maybe TyExpTreeDiagram)
annotateTypeBisim = Bisim { fLang  = id
                          , fNM    = fmap (toNM :: Term -> TyExpTreeDiagram) . fromNM
                          , alphaA = toNM
                          , alphaB = return . toNM }

-- Evaluation
evalBisim :: Bisimulation Term Term TyExpTreeDiagram (Maybe TyExpTreeDiagram)
evalBisim = mkInjBisim step