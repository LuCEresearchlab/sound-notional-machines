{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Lang.TypedArith.Generators where

import Hedgehog (MonadGen)

import NotionalMachines.Lang.TypedArith.Main         (TypedTerm, typedTerm)
import NotionalMachines.Lang.UntypedArith.Generators (genTerm)


genTypedTerm :: MonadGen m => m TypedTerm
genTypedTerm = typedTerm <$> genTerm
