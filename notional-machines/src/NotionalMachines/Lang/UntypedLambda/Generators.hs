{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Lang.UntypedLambda.Generators where

import           Hedgehog     hiding (Var)
import qualified Hedgehog.Gen as Gen

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), freeVs)

import NotionalMachines.Utils (genName)


genExp :: MonadGen m => m Exp
genExp =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> genName
    ] [
      -- recursive generators
      Gen.subtermM genExp (\x -> Lambda <$> genName <*> pure x)
    , Gen.subterm2 genExp genExp App
    ]

genCombinator :: Gen Exp
genCombinator = fmap bindFreeVars genExp

bindFreeVars :: Exp -> Exp
bindFreeVars e = foldl (\en var -> App (Lambda var en) (Lambda "a" (Var "a"))) e (freeVs e)

depth :: Exp -> Int
depth (Var _)      = 0
depth (Lambda _ e) = depth e + 1
depth (App e1 e2)  = max (depth e1 + 1) (depth e2 + 1)

