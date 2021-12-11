{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs where

import Control.Monad (liftM2)
import Data.Function ((&))

import NotionalMachines.Lang.UntypedLambda.Main    (Exp (..))
import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamilies, AlligatorFamily,
                                                    AlligatorFamilyF (..), Color, colorToName,
                                                    deBruijnAlligators, nameToColor)

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Injective    (Injective, fromNM, toNM)
import NotionalMachines.Meta.Steppable    (eval)

-------------------------
-- Lang to NM and back --
-------------------------
nmToLang' :: (a -> String) -> [AlligatorFamilyF a] -> Maybe Exp
nmToLang' toName families =
  -- TODO: improve this by resolving the typing weirdness of [Als]
  fmap f2e families & \case []           -> Nothing
                            [me]         -> me
                            me1:me2:rest -> foldl (liftM2 App) (liftM2 App me1 me2) rest
  where f2e (HungryAlligator c proteges) = Lambda (toName c) <$> nmToLang' toName proteges
        f2e (OldAlligator proteges)      = nmToLang' toName proteges
        f2e (Egg c)                      = Just (Var (toName c))

nmToLang :: [AlligatorFamily] -> Maybe Exp
nmToLang = nmToLang' colorToName

langToNm :: Exp -> [AlligatorFamily]
langToNm (Var name)              = [Egg (nameToColor name)]
langToNm (Lambda name e)         = [HungryAlligator (nameToColor name) (langToNm e)]
langToNm (App e1 e2 @ (App _ _)) = langToNm e1 ++ [OldAlligator (langToNm e2)]
langToNm (App e1 e2)             = langToNm e1 ++ langToNm e2

------------------

--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alphaA    alphaB
--    |          |
--    |          |
--
--    A' --f'--> B'

instance Injective Exp AlligatorFamilies where
  toNM   = langToNm
  fromNM = nmToLang

bisim :: Bisimulation Exp Exp [AlligatorFamilyF Color] [AlligatorFamilyF Int]
bisim = MkBisim { fLang  = eval
                , fNM    = deBruijnAlligators . eval
                , alphaA = toNM
                , alphaB = deBruijnAlligators . toNM }

-- Commutation proof:
-- alpha_B . f' == f . alpha_A

-- alphaBCmpf' :: A' -> B
-- -- alphaBCmpf' = alphaB . f'
-- alphaBCmpf' = langToNm . eval

-- fCmpalphaA :: A' -> B
-- -- fCmpalphaA = f . alphaA
-- fCmpalphaA = eval . langToNm

