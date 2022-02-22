{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Lang.TypedLambdaRef.Generators where

import           Hedgehog       (MonadGen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import NotionalMachines.Lang.TypedLambdaRef.Main (Term (..), Type (..))

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Store, Location, StateRacket (StateRacket), Name)
import NotionalMachines.Utils                              (genName)


genTerm :: MonadGen m => m Term
genTerm =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> genName
    , return Tru
    , return Fls
    , return Zero
    , return Unit
    -- , Loc <$> Gen.int (Range.linear 0 10)
    ] [
      -- recursive generators
      Gen.subtermM genTerm (\x -> Lambda <$> genName <*> genType <*> pure x)
    , Gen.subterm2 genTerm genTerm App
    , Gen.subterm3 genTerm genTerm genTerm If
    , Gen.subterm  genTerm Succ
    , Gen.subterm  genTerm Pred
    , Gen.subterm  genTerm IsZero
    , Gen.subterm  genTerm Ref
    , Gen.subterm  genTerm Deref
    , Gen.subterm2 genTerm genTerm Assign
    ]

genType :: MonadGen m => m Type
genType =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      return TyBool
    , return TyNat
    , return TyUnit
    ] [
      -- recursive generators
      Gen.subterm2 genType genType TyFun
    , Gen.subterm  genType TyRef
    ]

genLocation :: MonadGen m => m Location
genLocation = Gen.int (Range.constant 0 10)

genStore :: MonadGen m => m (Store Location)
genStore = Gen.map (Range.constant 0 5) genEntry
  where genEntry :: MonadGen m => m (Location, Term)
        genEntry = (,) <$> genLocation <*> genTerm

genNameEnv :: MonadGen m => m (Store Name)
genNameEnv = Gen.map (Range.constant 0 5) genEntry
  where genEntry :: MonadGen m => m (Name, Term)
        genEntry = (,) <$> genName <*> genTerm

genTermStore :: MonadGen m => m (Term, Store Location)
genTermStore = (,) <$> genTerm <*> genStore

genLocationStore :: MonadGen m => m (Location, Store Location)
genLocationStore = (,) <$> genLocation <*> genStore

genLocationTermStore :: MonadGen m => m ((Location, Term), Store Location)
genLocationTermStore = (,) <$> ((,) <$> genLocation <*> genTerm) <*> genStore

genStateRacket :: MonadGen m => m StateRacket
genStateRacket = StateRacket <$> genNameEnv <*> genStore

genTermStateRacket :: MonadGen m => m (Term, StateRacket)
genTermStateRacket = (,) <$> genTerm <*> genStateRacket
