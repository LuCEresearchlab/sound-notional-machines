{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

import           Hedgehog hiding (Var, eval, evalM)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Main (defaultMain)

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

-- import Control.Monad.IO.Class (liftIO)
-- import Control.Concurrent (threadDelay)
-- import Control.Concurrent.Async.Lifted (race)
-- import Control.Exception (evaluate)

import Data.Foldable (toList)
import Data.List (intersect)
import Data.Maybe (fromJust)

import UntypedLambda

import ExpressionTutorGenerator

import           ExpressionTutor hiding (bisim)
import qualified ExpressionTutor as ET  (bisim)
import           ExpTree hiding (bisim)
import qualified ExpTree as ETree  (bisim)
import           Reduct hiding (bisim)
import qualified Reduct as R   (bisim)
import           Alligator hiding (bisim)
import qualified Alligator as A   (bisim)

import AsciiAlligators

import qualified Injective as Inj
import qualified Bijective as Bij
import Steppable
import Bisimulation

import Utils

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
  -- classify "closed terms" $ null (freeVs e)
  -- classify "open terms" $ not (null (freeVs e))
  -- classify "depth 0" $ depth e == 0
  -- classify "depth <= 2" $ depth e <= 2
  -- classify "depth > 7" $ depth e > 7
  tripping e f' f

depth :: Exp -> Int
depth (Var name) = 0
depth (Lambda name e) = depth e + 1
depth (App e1 e2) = max (depth e1 + 1) (depth e2 + 1)

is_equivalent_to :: (Eq a, Show a) => (Exp -> a) -> (Exp -> a) -> Property
is_equivalent_to f f' = prop $ do
  e <- forAll genCombinator
  f e === f' e

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
            deBruijnAlligators [newA2] === deBruijnAlligators [a2]
            null (intersect (toList a1) (toList newA2)) === True
    _ -> success

game_play_example :: Property
game_play_example = prop $ do
  c1 <- forAll (Gen.enumBounded :: Gen Color)
  c2 <- forAll (Gen.enumBounded :: Gen Color)
  let rightGuess = guess [anot] [([atru], [afls]), ([afls], [atru])] [c1, c2]
  let rightColors = [c1, c2] == [Color 'c', Color 'b']
  (rightColors && rightGuess || not rightColors && not rightGuess) === True

----------------------

lambdaTest :: TestTree
lambdaTest = testGroup "Untyped Lambda Calculus" [
      testProperty "eval produces a value"
        eval_produces_value
    , testProperty "parse is left inverse of unparse" $
        parse `is_left_inverse_of` unparse
  ]

expressionTutorTest :: TestTree
expressionTutorTest = testGroup "Expressiontutor" [
      testProperty "nmToLang is left inverse of langToNm" $
        Inj.fromNM `is_left_inverse_of` (Inj.toNM :: Exp -> ExpTreeDiagram)
    , testProperty "commutation proof" $
        bisimulationCommutes ET.bisim
  ]

expTreeTest :: TestTree
expTreeTest = testGroup "Expression Trees" [
      testProperty "nmToLang is inverse of langToNm" $
        (Bij.fromNM . Bij.toNM) `is_equivalent_to` id
    , testProperty "commutation proof" $
        bisimulationCommutes ETree.bisim
  ]

reductTest :: TestTree
reductTest = testGroup "Reduct" [
      testProperty "nmToLang is left inverse of langToNm" $
        Inj.fromNM `is_left_inverse_of` (Inj.toNM :: Exp -> ReductExp)
    , testProperty "commutation proof" $
        bisimulationCommutes R.bisim
    , testProperty "reduct trees have unique ids"
        prop_uniqids
  ]

alligatorTest :: TestTree
alligatorTest = testGroup "Alligators" [
      testProperty "nmToLang is left inverse of langToNm" $
        Inj.fromNM `is_left_inverse_of` (Inj.toNM :: Exp -> AlligatorFamilies)
    , testProperty "commutation proof" $
        bisimulationCommutes A.bisim
    , testProperty "color rule"
        color_rule
    , testProperty "In example, right guess <=> right colors"
        game_play_example
    , testGroup "de Bruijn Alligators" (
      let f = fmap unparse . Alligator.nmToLang . deBruijnAlligators . Inj.toNM . fromJust . parse
      in [
          testCase "id" $ assertEqual ""
            [HungryAlligator 0 [Egg 0]] -- expected
            (deBruijnAlligators [HungryAlligator (Color 'a') [Egg (Color 'a')]])
        , testCase "c0" $ assertEqual ""
            (Just "(\\0.(\\0.0))") -- expected
            (f "\\s.\\z.z")
        , testCase "c2" $ assertEqual ""
            (Just "(\\0.(\\0.1 (1 0)))") -- expected
            (f "\\s.\\z.s(s z)")
        , testCase "plus" $ assertEqual ""
            (Just "(\\0.(\\0.(\\0.(\\0.3 1 (2 0 1)))))") -- expected
            (f "(\\m.(\\n.(\\s.\\z.m s (n z s))))")
        , testCase "fix" $ assertEqual ""
            (Just "(\\0.(\\0.1 (\\0.1 1 0)) (\\0.1 (\\0.1 1 0)))") -- expected
            (f "\\f.(\\x.f (\\y.(x x) y)) (\\x.f (\\y.(x x) y))")
        , testCase "foo" $ assertEqual ""
            (Just "(\\0.(\\0.0)) (\\0.0)") -- expected
            (f "(\\x.(\\x.x)) (\\x.x)")
      ])
    , testProperty "asciiAlligator nm == asciiAlligators lambda" $
       (show . toAscii . (Inj.toNM :: Exp -> AlligatorFamilies)) `is_equivalent_to` (show . toAscii)
  ]

tests :: TestTree
tests = testGroup "Tests" [lambdaTest, expressionTutorTest, expTreeTest, reductTest, alligatorTest]

defaultNumberOfTests = 300

main = Test.Tasty.defaultMain tests

