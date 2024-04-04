{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}


module NotionalMachines.LangInMachine.TypedLambdaArrayParkingSpaces where

import Data.Bifunctor (bimap)
import Control.Monad.State.Lazy (lift, StateT, get, modify, runStateT, evalStateT)

import Prettyprinter      (pretty)


import qualified NotionalMachines.Machine.ArrayAsParkingSpaces.Main as NM
import qualified NotionalMachines.Lang.TypedLambdaArray.AbstractSyntax as Lang

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Steppable (traceM, evalM)
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



alpha_A :: (Lang.Term, Lang.Store Lang.Location) -> NM.Term
alpha_A (e, s) =
-- alpha_A :: StateT (Lang.Store Lang.Location) (Either Error) Lang.Term -> NM.Term
-- alpha_A t = case runStateT t Lang.emptyStore of
--   Left er -> error "can't get started: " ++ show (pretty er)
--   Right (e, s) ->
      case Lang.redex e of
          Lang.ArrayAlloc ty l | Lang.isValue l ->
              let id = Lang.nextLocation s
               in NM.ArrayAlloc (tyLangToNM ty) (fromIntegral (Lang.peanoToDec l)) (show id)
          Lang.ArrayAccess (Lang.Loc l) i | Lang.isValue i ->
              NM.ArrayAccess (NM.Loc l) (fromIntegral (Lang.peanoToDec i))
          Lang.ArrayAccess Lang.Null i | Lang.isValue i ->
              NM.ArrayAccess NM.Null (fromIntegral (Lang.peanoToDec i))
          Lang.Assign (Lang.ArrayAccess (Lang.Loc l) i) v | Lang.isValue v ->
              NM.ArrayWrite (NM.Loc l) (fromIntegral (Lang.peanoToDec i)) (rec v)
          Lang.Assign (Lang.ArrayAccess Lang.Null i) v | Lang.isValue v ->
              NM.ArrayWrite NM.Null (fromIntegral (Lang.peanoToDec i)) (rec v)
          Lang.Loc l -> NM.Loc l
          -- Numbers
          Lang.Zero -> NM.PrimValue "0"
          n@(Lang.Succ e') | Lang.isValue e' -> NM.PrimValue . show . Lang.peanoToDec $ n
          -- Booleans
          Lang.Tru -> NM.PrimValue "true"
          Lang.Fls -> NM.PrimValue "false"
          _ -> NM.Other
  where rec t = alpha_A (t, s)

alpha_A_trace :: Lang.Term -> Either NM.Problem [NM.Term]
alpha_A_trace = bimap convertIssue (map alpha_A) . trace
  where trace :: Lang.Term -> Either Error [(Lang.Term, Lang.Store Lang.Location)]
        trace = mapM (`runStateT` Lang.emptyStore) . traceM


alpha_B_step :: StateT (Lang.Store Lang.Location) (Either Error) Lang.Term -> StateT NM.ParkingSpace (Either NM.Problem) NM.Term
alpha_B_step e = case evalStateT e Lang.emptyStore of
  Left er -> lift . Left . convertIssue $ er
  Right t -> return $ alpha_A (t, Lang.emptyStore)
  --Right (t, _) -> return $ alpha_A (return t)

alpha_B_eval :: Either Error Lang.Term -> Either NM.Problem NM.Term
alpha_B_eval = bimap convertIssue (alpha_A . (, Lang.emptyStore))


convertIssue :: Error -> NM.Problem
convertIssue = NM.Problem . show . pretty



bisimEval :: Bisimulation Lang.Term
                          (Either Error      Lang.Term)
                          (Either NM.Problem [NM.Term])
                          (Either NM.Problem NM.Term)
bisimEval = MkBisim { fLang = Lang.evalM'
                    , fNM = f_NM
                    , alphaA = alpha_A_trace
                    , alphaB = alpha_B_eval
                    }
  where f_NM :: Either NM.Problem [NM.Term] -> Either NM.Problem NM.Term
        f_NM t = t >>= ((`evalStateT` NM.emptyParkingSpace) . NM.fNM_Replay)

