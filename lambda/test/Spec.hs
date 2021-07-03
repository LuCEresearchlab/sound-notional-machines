{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

import           Hedgehog hiding (Var, eval, evalM)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Main (defaultMain)

import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async.Lifted (race)
import           Control.Exception (evaluate)

import Data.Foldable (toList)
import Data.List (intersect)

import UntypedLambda
import           ExpressionTutor hiding (bisim)
import qualified ExpressionTutor as ET  (bisim)
import           Reduct hiding (bisim)
import qualified Reduct as R   (bisim)
import           Alligator hiding (bisim)
import qualified Alligator as A   (bisim)

import Utils
import AsciiAlligators

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
  (isValue <$> evalM e) === Just True

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
    , ("parse is left inverse of unparse:",
        parse `is_left_inverse_of` unparse)
  ]

expressionTutorTest :: Group
expressionTutorTest = Group "Expressiontutor" [
      ("nmToLang is left inverse of langToNm:",
        fromNM `is_left_inverse_of` (toNM :: Exp -> ExpTreeDiagram))
    , ("commutation proof:",
        bisimulationCommutes ET.bisim)
  ]

reductTest :: Group
reductTest = Group "Reduct" [
      ("nmToLang is left inverse of langToNm:",
        fromNM `is_left_inverse_of` (toNM :: Exp -> ReductExp))
    , ("commutation proof:",
        bisimulationCommutes R.bisim)
    , ("reduct trees have unique ids:",
        prop_uniqids)
  ]

alligatorTest :: Group
alligatorTest = Group "Alligators" [
      ("nmToLang is left inverse of langToNm:",
        fromNM `is_left_inverse_of` (toNM :: Exp -> AlligatorFamilies))
    , ("commutation proof:",
        bisimulationCommutes A.bisim)
    , ("color rule:",
        color_rule)
    , ("asciiAlligator . langToNm is equivalente to directly from Exp:",
       (show . toAscii . (toNM :: Exp -> AlligatorFamilies)) `is_equivalent_to` (show . toAscii))
  ]


defaultNumberOfTests = 300

main :: IO ()
main = defaultMain $ fmap checkParallel [lambdaTest, expressionTutorTest, reductTest, alligatorTest]

