{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}


module NotionalMachines.LangInMachine.TypedArithExpressionTutor where

import Control.Monad.State.Lazy (State, StateT (..), lift, liftM3)

import Data.Set (Set)

import           NotionalMachines.Lang.TypedArith.Main         (TyTerm (TyFls, TyIf, TyIsZero, TyPred, TySucc, TyTru, TyZero),
                                                                Type (TyBool, TyNat), TypedTerm,
                                                                typeof, typeof1)
import qualified NotionalMachines.Lang.TypedArith.Main         as TypedArith (Type)
import           NotionalMachines.Lang.UntypedArith.Main       (Term)
import           NotionalMachines.Machine.ExpressionTutor.Main hiding (Type)
import qualified NotionalMachines.Machine.ExpressionTutor.Main as ET (Type (..))

import NotionalMachines.LangInMachine.UntypedArithExpressionTutor ()

import NotionalMachines.Meta.Bisimulation (Bisimulation (..), mkInjBisimM)
import NotionalMachines.Meta.Injective    (Injective, fromNM)
import NotionalMachines.Meta.LangToNM     (LangToNM (..))

import NotionalMachines.Util.Util (eitherToMaybe)

------------------------
-- Lang Types to NM Type representation and back
------------------------

tyToNM :: TypedArith.Type -> ET.Type
tyToNM = ET.MkTy . \case TyBool -> "Bool"
                         TyNat  -> "Int"

tyNMToTy :: ET.Type -> Maybe TypedArith.Type
tyNMToTy = \case ET.MkTy "Bool" -> return TyBool
                 ET.MkTy "Int"  -> return TyNat
                 _              -> Nothing

instance LangToNM TypedArith.Type ET.Type where
  toNM = tyToNM

-- Ask for the type of a diagram not annotated with types
typeOfBisim :: Bisimulation Term (Maybe TypedArith.Type) ExpTutorDiagram (Maybe ET.Type)
typeOfBisim = mkInjBisimM _fLang
  where _fLang :: Term -> Maybe TypedArith.Type
        _fLang = eitherToMaybe . typeof

----------------
----------------

pattern NodeTrue   :: Maybe ET.Type -> Int -> Node
pattern NodeFalse  :: Maybe ET.Type -> Int -> Node
pattern NodeIf     :: Maybe ET.Type -> Int -> Node
pattern NodeZero   :: Maybe ET.Type -> Int -> Node
pattern NodeSucc   :: Maybe ET.Type -> Int -> Node
pattern NodePred   :: Maybe ET.Type -> Int -> Node
pattern NodeIsZero :: Maybe ET.Type -> Int -> Node

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

langToNM :: TypedTerm -> ExpTutorDiagram
langToNM = langToET go
  where go :: TypedTerm -> State Int ExpTutorDiagram
        go s = case s of
          (TyTru, ty)         -> newDiaLeaf   (NodeTrue   (fmap tyToNM ty))
          (TyFls, ty)         -> newDiaLeaf   (NodeFalse  (fmap tyToNM ty))
          (TyIf t1 t2 t3, ty) -> newDiaBranch (NodeIf     (fmap tyToNM ty)) go [t1, t2, t3]
          (TyZero, ty)        -> newDiaLeaf   (NodeZero   (fmap tyToNM ty))
          (TySucc t1, ty)     -> newDiaBranch (NodeSucc   (fmap tyToNM ty)) go [t1]
          (TyPred t1, ty)     -> newDiaBranch (NodePred   (fmap tyToNM ty)) go [t1]
          (TyIsZero t1, ty)   -> newDiaBranch (NodeIsZero (fmap tyToNM ty)) go [t1]

nmToLang :: ExpTutorDiagram -> Maybe TypedTerm
nmToLang = etToLang go
  where
    go :: ExpTutorDiagram -> StateT (Set Int) Maybe TypedTerm
    go d = checkCycle d $ case d of
      DiaLeaf   (NodeTrue   ty _)              -> return (TyTru, tyNMToTy =<< ty)
      DiaLeaf   (NodeFalse  ty _)              -> return (TyFls, tyNMToTy =<< ty)
      DiaBranch (NodeIf     ty _) ts@[_, _, _] -> let [n1, n2, n3] = fmap go ts
                                                   in (, tyNMToTy =<< ty) <$> liftM3 TyIf n1 n2 n3
      DiaLeaf   (NodeZero   ty _)              -> return (TyZero, tyNMToTy =<< ty)
      DiaBranch (NodeSucc   ty _) [t]          -> (, tyNMToTy =<< ty) . TySucc   <$> go t
      DiaBranch (NodePred   ty _) [t]          -> (, tyNMToTy =<< ty) . TyPred   <$> go t
      DiaBranch (NodeIsZero ty _) [t]          -> (, tyNMToTy =<< ty) . TyIsZero <$> go t
      _ -> lift Nothing -- "incorrect diagram"


instance LangToNM TypedTerm ExpTutorDiagram where
  toNM = langToNM

instance Injective TypedTerm ExpTutorDiagram Maybe where
  fromNM = nmToLang

-- Annotate diagram with types
annotateTypeBisim :: Bisimulation TypedTerm (Maybe TypedTerm) ExpTutorDiagram (Maybe ExpTutorDiagram)
annotateTypeBisim = mkInjBisimM _fLang
  where _fLang :: TypedTerm -> Maybe TypedTerm
        _fLang = eitherToMaybe . typeof1
