{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}


module NotionalMachines.LangInMachine.TypedLambdaArrayParkingSpaces where

import Data.Bifunctor (bimap)
import Control.Monad ((<=<))
import Control.Monad.State.Lazy (evalStateT)

import Prettyprinter      (pretty)


import qualified NotionalMachines.Machine.ArrayAsParkingSpaces.Main as NM
import qualified NotionalMachines.Lang.TypedLambdaArray.AbstractSyntax as Lang
import           NotionalMachines.Lang.TypedLambdaArray.Main

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Lang.Error (Error (..))



tyLangToNM :: Lang.Type -> NM.Type
tyLangToNM = \case Lang.TyFun _ _  -> NM.TyOtherPrim
                   Lang.TyUnit     -> NM.TyOtherPrim
                   Lang.TyRef ty   -> NM.TyRef (tyLangToNM ty)
                   Lang.TyArray ty -> NM.TyRef (tyLangToNM ty)
                   Lang.TyNull     -> NM.TyOtherPrim
                   Lang.TyBool     -> NM.TyBool
                   Lang.TyNat      -> NM.TyNat
                   Lang.TyVar _    -> NM.TyOtherPrim
                   Lang.TyTuple _  -> NM.TyOtherPrim


langToNM :: (Lang.Term, Lang.Store Lang.Location) -> NM.Term
langToNM (e, s) =
      case Lang.redex e of
          Lang.ArrayAlloc ty l | Lang.isValue l ->
              NM.ArrayAlloc (tyLangToNM ty) (fromIntegral (Lang.peanoToDec l)) (show (Lang.nextLocation s))
          Lang.ArrayAccess (Lang.Loc l) i | Lang.isValue i ->
              NM.ArrayAccess (NM.Loc l) (fromIntegral (Lang.peanoToDec i))
          Lang.ArrayAccess Lang.Null i | Lang.isValue i ->
              NM.ArrayAccess NM.Null (fromIntegral (Lang.peanoToDec i))
          Lang.Assign (Lang.ArrayAccess (Lang.Loc l) i) v | Lang.isValue v ->
              NM.ArrayWrite (NM.Loc l) (fromIntegral (Lang.peanoToDec i)) (rec v)
          Lang.Assign (Lang.ArrayAccess Lang.Null i) v | Lang.isValue v ->
              NM.ArrayWrite NM.Null (fromIntegral (Lang.peanoToDec i)) (rec v)
          -- Values
          -- Location
          Lang.Loc l -> NM.Loc l
          -- Numbers
          Lang.Zero -> NM.PrimValue "0"
          n@(Lang.Succ e') | Lang.isValue e' -> NM.PrimValue . show . Lang.peanoToDec $ n
          -- Booleans
          Lang.Tru -> NM.PrimValue "true"
          Lang.Fls -> NM.PrimValue "false"
          -- Unit
          Lang.Unit -> NM.Unit
          -- Null
          Lang.Null -> NM.Null
          -- Arrays can't contain values of other types
          _ -> NM.Other
  where rec t = langToNM (t, s)


convertIssue :: Error -> NM.Problem
convertIssue = NM.Problem . show . pretty





alpha_A :: Lang.Term -> Either NM.Problem [NM.Term]
alpha_A = bimap convertIssue (map langToNM . rawTrace) . trace' Lang.emptyStore id

alpha_B :: Either Error Lang.Term -> Either NM.Problem NM.Term
alpha_B = bimap convertIssue (langToNM . (, Lang.emptyStore))


bisimEval :: Bisimulation Lang.Term
                          (Either Error      Lang.Term)
                          (Either NM.Problem [NM.Term])
                          (Either NM.Problem NM.Term)
bisimEval = MkBisim { fLang = Lang.evalM' . fst <=< Lang.typecheck
                    , fNM = f_NM
                    , alphaA = alpha_A
                    , alphaB = alpha_B
                    }
  where f_NM :: Either NM.Problem [NM.Term] -> Either NM.Problem NM.Term
        f_NM t = t >>= ((`evalStateT` NM.emptyParkingSpace) . NM.fNM_Replay)
