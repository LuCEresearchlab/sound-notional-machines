{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}

module NotionalMachines.Lang.TypedLambdaArray.Generators where

import           Hedgehog       (MonadGen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import NotionalMachines.Lang.TypedLambdaArray.Main (Term (..), Type (..))

import NotionalMachines.Lang.TypedLambdaArray.AbstractSyntax (Location, StateRacket, Store,
                                                            emptyStateAlaRacket)
import NotionalMachines.Util.Generators                    (genName)


genTerm :: MonadGen m => m Term
genTerm =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> genName
    , return Tru
    , return Fls
    , return Zero
    , return Unit
    , return Null
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
    , Gen.subtermM genTerm (\x -> ArrayAlloc <$> genType <*> pure x)
    , Gen.subterm2 genTerm genTerm ArrayAccess
    , Tuple <$> Gen.list (Range.linear 0 5) genTerm
    , Gen.subtermM genTerm (\x -> Proj <$> Gen.integral (Range.linear 0 10) <*> pure x)
    ]

genType :: MonadGen m => m Type
genType =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      return TyBool
    , return TyNat
    , return TyUnit
    , return TyNull
    ] [
      -- recursive generators
      Gen.subterm2 genType genType TyFun
    , Gen.subterm  genType TyRef
    , Gen.subterm  genType TyArray
    , TyTuple <$> Gen.list (Range.linear 0 5) genType
    ]

genLocation :: MonadGen m => m Location
genLocation = Gen.int (Range.linear 0 10)

genStore :: MonadGen m => m (Store Location)
genStore = Gen.map (Range.linear 0 5) genEntry
  where genEntry :: MonadGen m => m (Location, Term)
        genEntry = (,) <$> genLocation <*> genTerm

genTermStore :: MonadGen m => m (Term, Store Location)
genTermStore = (,) <$> genTerm <*> genStore

genTermStateRacket :: MonadGen m => m (Term, StateRacket)
genTermStateRacket = ( , emptyStateAlaRacket) <$> genTerm
