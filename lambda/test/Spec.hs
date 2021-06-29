{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

import           Hedgehog hiding (Var, eval, evalMaybe)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Main (defaultMain)

import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async.Lifted (race)
import           Control.Exception (evaluate)

import Data.Foldable (toList)
import Data.List (intersect)

import UntypedLambda
import           ExpressionTutor hiding (bisim, nmToLang, langToNm)
import qualified ExpressionTutor as ET  (bisim, nmToLang, langToNm)
import           Reduct hiding (bisim, nmToLang, langToNm)
import qualified Reduct as R   (bisim, nmToLang, langToNm)
import           Alligator hiding (bisim, nmToLang, langToNm)
import qualified Alligator as A   (bisim, nmToLang, langToNm)

import Utils

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

bisimulationCommutes :: (Eq b, Show b) => Bisimulation Exp b' a b -> Property
bisimulationCommutes b = (alphaB b . fLang b) `is_equivalent_to` (fNM b . alphaA b)

----- Reduct -----

prop_uniqids :: Property
prop_uniqids = prop $ do
  e <- forAll genReductExp
  let ids = uids $ updateUids 0 e
  ids === [0..((length ids) - 1)]

uids = toList

----- Alligator -----

color_rule :: Property
color_rule = prop $ do
  e1 <- forAll genExp
  case alphaA A.bisim e1 of
    a1:a2:_ ->
      let newA2 = recolor a1 a2
      in do annotateShow a1
            annotateShow a2
            annotateShow newA2
            colorsToInts [newA2] === colorsToInts [a2]
            intersect (toList a1) (toList newA2) === []
    _ -> success

----------------------


lambdaTest :: Group
lambdaTest = Group "Lambda" [
      ("eval produces a value:",
        eval_produces_value)
    , ("eval is equivalent to bigStep:",
        evalMaybe `is_equivalent_to` bigStepMaybe)
    , ("parse is left inverse of unparse:",
        parse `is_left_inverse_of` unparse)
  ]

expressionTutorTest :: Group
expressionTutorTest = Group "Expressiontutor" [
      ("nmToLang is left inverse of langToNm:",
        ET.nmToLang `is_left_inverse_of` ET.langToNm)
    , ("commutation proof:",
        bisimulationCommutes ET.bisim)
  ]

reductTest :: Group
reductTest = Group "Reduct" [
      ("nmToLang is left inverse of langToNm:",
        R.nmToLang `is_left_inverse_of` R.langToNm)
    , ("commutation proof:",
        bisimulationCommutes R.bisim)
    , ("reduct trees have unique ids:",
        prop_uniqids)
  ]

alligatorTest :: Group
alligatorTest = Group "Alligators" [
      ("nmToLang is left inverse of langToNm:",
        A.nmToLang `is_left_inverse_of` A.langToNm)
    , ("commutation proof:",
        bisimulationCommutes A.bisim)
    , ("color rule:",
        color_rule)
    , ("asciiAlligator . langToNm is equivalente to directly from Exp:",
       (prettyAlligators . A.langToNm) `is_equivalent_to` exp2AlligatorAscii)
  ]


defaultNumberOfTests = 500

main :: IO ()
main = defaultMain $ fmap checkParallel [lambdaTest, expressionTutorTest, reductTest, alligatorTest]

