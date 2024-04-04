{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}

module NotionalMachines.LangInMachine.TypedLambdaArrayParkingSpaces where

import Control.Monad.State.Lazy (StateT, get, modify)

import qualified NotionalMachines.Machine.ArrayAsParkingSpaces.Main as NM
import qualified NotionalMachines.Lang.TypedLambdaArray.AbstractSyntax as Lang
import NotionalMachines.Lang.Error     (Error (..))



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



alpha_A :: Lang.Term -> NM.Term
alpha_A e = case Lang.redex e of
  Lang.ArrayAlloc ty l | Lang.isValue l ->
      NM.ArrayAlloc (tyLangToNM ty) (fromIntegral (Lang.peanoToDec l))
  Lang.ArrayAccess (Lang.Loc l) i | Lang.isValue i ->
      NM.ArrayAccess (NM.Loc l) (fromIntegral (Lang.peanoToDec i))
  Lang.ArrayAccess Lang.Null i | Lang.isValue i ->
      NM.ArrayAccess NM.Null (fromIntegral (Lang.peanoToDec i))
  Lang.Assign (Lang.ArrayAccess (Lang.Loc l) i) v | Lang.isValue v ->
      NM.ArrayWrite (NM.Loc l) (fromIntegral (Lang.peanoToDec i)) (alpha_A v)
  Lang.Assign (Lang.ArrayAccess Lang.Null i) v | Lang.isValue v ->
      NM.ArrayWrite NM.Null (fromIntegral (Lang.peanoToDec i)) (alpha_A v)
  -- Numbers
  Lang.Zero -> NM.PrimValue "0"
  n@(Lang.Succ e') | Lang.isValue e' -> NM.PrimValue . show . Lang.peanoToDec $ n
  -- Booleans
  Lang.Tru -> NM.PrimValue "true"
  Lang.Fls -> NM.PrimValue "false"
  _ -> NM.Other


alpha_B :: StateT (Lang.Store Lang.Location) (Either Error) Lang.Term -> StateT NM.ParkingSpace (Either NM.Problem) NM.Term
alpha_B = undefined
