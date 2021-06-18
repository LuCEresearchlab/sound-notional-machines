{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

import           Hedgehog hiding (Var, eval, evalMaybe)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Main (defaultMain)

import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async.Lifted (race)
import           Control.Exception (evaluate)

import Data.Foldable (toList)

import UntypedLambda
import           ExpressionTutor hiding (nm2lang, lang2nm, alphaA, alphaB, f, f')
import qualified ExpressionTutor as ET  (nm2lang, lang2nm, alphaA, alphaB, f, f')
import           Reduct hiding (nm2lang, lang2nm, alphaA, alphaB, f, f')
import qualified Reduct as R   (nm2lang, lang2nm, alphaA, alphaB, f, f')
import           Alligator hiding (nm2lang, lang2nm, alphaA, alphaB, f, f')
import qualified Alligator as A   (nm2lang, lang2nm, alphaA, alphaB, f, f')

import ExpressionTutorGenerator

----- Timeout -----
withTimeLimit :: Int -> TestT IO a -> TestT IO a
withTimeLimit timeout v = do
  result <-
    race
      (liftIO $ threadDelay timeout)
      v
  case result of
    Left () -> fail "Timeout exceeded"
    Right x -> pure x


----------------------
----- Generators -----
----------------------

----- Reduct -----

genReductExp :: MonadGen m => m ReductExp
genReductExp =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Pipe <$> genName <*> pure 0
    ] [
      -- recursive generators
      Gen.subtermM genReductExp (\x -> HolePipe <$> genName <*> Gen.maybe (pure x) <*> pure 0)
    , Gen.subtermM2 genReductExp genReductExp
        (\x y -> HolePlug <$> Gen.maybe (pure x) <*> Gen.maybe (pure y) <*> pure 0)
    ]


----------------------
----- Properties -----
----------------------

prop = withTests defaultNumberOfTests . property

----- Lambda -----

eval_produces_value :: Property
eval_produces_value = prop $ do
  e <- forAll genCombinator
  (isValue <$> evalMaybe e) === Just True

isValue :: Exp -> Bool
isValue Lambda {} = True
isValue _         = False

is_left_inverse_of :: Show a => (a -> Maybe Exp) -> (Exp -> a) -> Property
is_left_inverse_of f f' = prop $ do
  e <- forAll genExp
  tripping e f' f

is_equivalent_to :: (Eq a, Show a) => (Exp -> a) -> (Exp -> a) -> Property
is_equivalent_to f f' = prop $ do
  e <- forAll genCombinator
  test $ withTimeLimit 500000 $ f e === f' e

----- Reduct -----

prop_uniqids :: Property
prop_uniqids = prop $ do
  e <- forAll genReductExp
  let ids = uids $ updateUids 0 e
  ids === [1..(length ids)]

uids = toList

----------------------


lambdaTest :: Group
lambdaTest = Group "Lambda" [
      ("eval produces a value:", eval_produces_value)
    , ("eval is equivalent to bigStep:", evalMaybe `is_equivalent_to` bigStepMaybe)
    , ("parse is left inverse of unparse:", parse `is_left_inverse_of` unparse)
  ]

expressionTutorTest :: Group
expressionTutorTest = Group "Expressiontutor" [
      ("nm2lang is left inverse of lang2nm:", ET.nm2lang `is_left_inverse_of` ET.lang2nm)
    , ("commutation proof:", (ET.alphaB . ET.f') `is_equivalent_to` (ET.f . ET.alphaA))
  ]

reductTest :: Group
reductTest = Group "Reduct" [
      ("nm2lang is left inverse of lang2nm:", R.nm2lang `is_left_inverse_of` R.lang2nm)
    , ("commutation proof:", (R.alphaB . R.f') `is_equivalent_to` (R.f . R.alphaA))
    , ("reduct trees have unique ids:", prop_uniqids)
  ]

alligatorTest :: Group
alligatorTest = Group "Alligators" [
      ("nm2lang is left inverse of lang2nm:", A.nm2lang `is_left_inverse_of` A.lang2nm)
    , ("commutation proof:", (restartColors . A.alphaB . A.f') `is_equivalent_to` (restartColors . A.f . A.alphaA))
    , ("asciiAlligator . lang2nm is equivalente to directly from Exp:", (prettyAlligators . A.lang2nm) `is_equivalent_to` exp2AlligatorAscii)
  ]


defaultNumberOfTests = 500

main :: IO ()
main = defaultMain $ fmap checkParallel [lambdaTest, expressionTutorTest, reductTest, alligatorTest]

