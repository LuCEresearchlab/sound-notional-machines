{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Lang.UntypedArith.Generators where

import           Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen

import NotionalMachines.Lang.UntypedArith.Main (Term(..))


genTerm :: MonadGen m => m Term
genTerm =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      return Tru
    , return Fls
    , return Zero
    ] [
      -- recursive generators
      Gen.subterm3 genTerm genTerm genTerm If
    , Gen.subterm  genTerm Succ
    , Gen.subterm  genTerm Pred
    , Gen.subterm  genTerm IsZero
    ]
