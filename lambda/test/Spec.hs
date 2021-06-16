{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

import           Hedgehog hiding (Var, eval, evalMaybe)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Main (defaultMain)

import Data.Foldable (toList)

import UntypedLambda
import           ExpressionTutor hiding (nm2lang, lang2nm, alphaA, alphaB, f, f')
import qualified ExpressionTutor as ET  (nm2lang, lang2nm, alphaA, alphaB, f, f')
import           Reduct hiding (nm2lang, lang2nm, alphaA, alphaB, f, f')
import qualified Reduct as R   (nm2lang, lang2nm, alphaA, alphaB, f, f')

import ExpressionTutorGenerator

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

----- Lambda -----

eval_produces_value :: Property
eval_produces_value = property $ do
  e <- forAll genCombinator
  (isValue <$> evalMaybe e) === Just True

isValue :: Exp -> Bool
isValue Lambda {} = True
isValue _         = False

is_left_inverse_of :: Show a => (Exp -> a) -> (a -> Maybe Exp) -> Property
is_left_inverse_of f f' = property $ do
  e <- forAll genExp
  tripping e f f'

is_equivalent_to :: (Eq a, Show a) => (Exp -> a) -> (Exp -> a) -> Property
is_equivalent_to f f' = property $ do
  e <- forAll genCombinator
  f e === f' e

----- Reduct -----

prop_uniqids :: Property
prop_uniqids = property $ do
  e <- forAll genReductExp
  let ids = uids $ updateUids 0 e
  ids === [1..(length ids)]

uids = toList

----------------------


lambdaTest :: IO Bool
lambdaTest = checkParallel $ Group "Lambda" [
      ("eval produces a value:", eval_produces_value)
    , ("eval is equivalent to bigStep:", evalMaybe `is_equivalent_to` bigStepMaybe)
    , ("parse is left inverse of unparse:", unparse `is_left_inverse_of` parse)
  ]

expressionTutorTest :: IO Bool
expressionTutorTest = checkParallel $ Group "Expressiontutor" [
      ("lang2nm is left inverse of nm2lang:", ET.lang2nm `is_left_inverse_of` ET.nm2lang)
    , ("commutation proof:", (ET.alphaB . ET.f') `is_equivalent_to` (ET.f . ET.alphaA))
  ]

reductTest :: IO Bool
reductTest = checkParallel $ Group "Reduct" [
      ("lang2nm is left inverse of nm2lang:", R.lang2nm `is_left_inverse_of` R.nm2lang)
    , ("commutation proof:", (R.alphaB . R.f') `is_equivalent_to` (R.f . R.alphaA))
    , ("reduct trees have unique ids:", prop_uniqids)
  ]


main :: IO ()
main = defaultMain [lambdaTest, expressionTutorTest, reductTest]

