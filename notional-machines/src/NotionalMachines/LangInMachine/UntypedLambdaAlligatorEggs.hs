{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs where


import NotionalMachines.Lang.UntypedLambda.Main    (Exp (..))
import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamily,
                                                    AlligatorFamilyF (..), Color,
                                                    deBruijnAlligators, nameToColor)

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Steppable    (eval)

-------------------------
-- Lang to NM and back --
-------------------------
langToNm :: Exp -> [AlligatorFamily]
langToNm (Var name)              = [Egg (nameToColor name)]
langToNm (Lambda name e)         = [HungryAlligator (nameToColor name) (langToNm e)]
langToNm (App e1 e2 @ (App _ _)) = langToNm e1 ++ [OldAlligator (langToNm e2)]
langToNm (App e1 e2)             = langToNm e1 ++ langToNm e2

bisim :: Bisimulation Exp Exp [AlligatorFamilyF Color] [AlligatorFamilyF Int]
bisim = MkBisim { fLang  = eval
                , fNM    = deBruijnAlligators . eval
                , alphaA = langToNm
                , alphaB = deBruijnAlligators . langToNm }

