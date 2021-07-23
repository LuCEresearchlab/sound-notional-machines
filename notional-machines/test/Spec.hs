{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

import           Hedgehog hiding (Var, eval, evalM)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Main (defaultMain)

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import Data.Foldable (toList)
import Data.List (intersect)
import Data.Maybe (fromJust, isNothing)
import Data.Either (either, isRight, isLeft)
import Data.Functor.Identity (Identity)


import qualified NotionalMachines.Lang.UntypedLambda as Lambda
import qualified NotionalMachines.Lang.UntypedLambdaExpressionTutor as LambdaET (bisim)
import           NotionalMachines.Lang.UntypedLambdaGenerator

import qualified NotionalMachines.Lang.Arith as Arith
import qualified NotionalMachines.Lang.TypedArith as TypedArith
import qualified NotionalMachines.Lang.ArithExpressionTutor as ArithET (bisim)
import           NotionalMachines.Lang.ArithGenerator as ArithGen

import qualified NotionalMachines.Lang.TypedLambdaArith as TypedLambda
-- import qualified NotionalMachines.Lang.TypedLambdaExpressionTutor as LambdaET (bisim)
import           NotionalMachines.Lang.TypedLambdaArithGenerator as TypedLambdaGen

import           NotionalMachines.Machine.ExpressionTutor
import           NotionalMachines.Machine.ExpTree hiding (bisim)
import qualified NotionalMachines.Machine.ExpTree as ETree  (bisim)
import           NotionalMachines.Machine.Reduct hiding (bisim)
import qualified NotionalMachines.Machine.Reduct as R   (bisim)
import           NotionalMachines.Machine.Alligator hiding (bisim)
import qualified NotionalMachines.Machine.Alligator as A   (bisim, nmToLang)
import           NotionalMachines.Machine.AsciiAlligators

import           NotionalMachines.Meta.Bisimulation
import           NotionalMachines.Meta.Steppable
import qualified NotionalMachines.Meta.Injective as Inj
import qualified NotionalMachines.Meta.Bijective as Bij

import NotionalMachines.Utils

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

-------- Alligators ----------

genColor :: MonadGen m => m Color
genColor = Color <$> (Gen.list (Range.singleton 1) $ Gen.element ['a'..'z'])


----------------------
----- Properties -----
----------------------

prop = withTests defaultNumberOfTests . property

----- Lambda -----

eval_produces_value :: Property
eval_produces_value = prop $ do
  e <- forAll genCombinator
  (Lambda.isValue <$> evalM e) === Just True

type_safety :: Property
type_safety = prop $ do {
  e <- forAll TypedLambdaGen.genTerm;
  classify "type checks" $ (isRight . TypedLambda.typeof) e;
  classify "eval to val" $ Just True == (TypedLambda.isValue <$> evalM e);
  annotateShow $ TypedLambda.typeof e;
  annotateShow $ evalM e;
  -- type-checks implies evals to value (doesn't get stuck)
  (    ((isRight . TypedLambda.typeof) e && (TypedLambda.isValue . fromJust . evalM) e)
    || ((isLeft  . TypedLambda.typeof) e && (isNothing . evalM) e)
  ) === True }

is_left_inverse_of :: (Show a, Show b, Eq b) => Gen b -> (a -> Maybe b) -> (b -> a) -> Property
is_left_inverse_of g f f' = prop $ do
  e <- forAll g
  -- classify "closed terms" $ null (freeVs e)
  -- classify "open terms" $ not (null (freeVs e))
  -- classify "depth 0" $ depth e == 0
  -- classify "depth <= 2" $ depth e <= 2
  -- classify "depth > 7" $ depth e > 7
  tripping e f' f

is_equivalent_to :: (Eq a, Show a, Show e) => Gen e -> (e -> a) -> (e -> a) -> Property
is_equivalent_to g f f' = prop $ do
  e <- forAll g
  f e === f' e

bisimulationCommutes :: (Eq b, Show b, Show a') => Gen a' -> Bisimulation a' b' a b -> Property
bisimulationCommutes g b = is_equivalent_to g (alphaB b . fLang b) (fNM b . alphaA b)

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
  c1 <- forAll genColor
  c2 <- forAll genColor
  let rightGuess = guess [anot] [([atru], [afls]), ([afls], [atru])] [c1, c2]
  let rightColors = [c1, c2] == [Color "c", Color "b"]
  (rightColors && rightGuess || not rightColors && not rightGuess) === True

----------------------

lambdaTest :: TestTree
lambdaTest = testGroup "Untyped Lambda Calculus" [
      testProperty "eval produces a value"
        eval_produces_value
    , testProperty "parse is left inverse of unparse" $
        is_left_inverse_of genExp Lambda.parse Lambda.unparse
  ]

typLambdaTest :: TestTree
typLambdaTest = testGroup "Typed Lambda Calculus" [
      testProperty "parse is left inverse of unparse" $
        is_left_inverse_of TypedLambdaGen.genTerm (eitherToMaybe . TypedLambda.parse) TypedLambda.unparse
    , testProperty "language is type safe"
        type_safety
    , testGroup "Type checking" [
          testCase "if iszero 0 then 0 else pred 0 : Nat" $ assertEqual ""
            (Right TypedLambda.TyNat) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if iszero 0 then 0 else pred 0")
        , testCase "if true then 0 else false : ??" $ assertEqual ""
            (Left "type error") -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if true then 0 else false")
        , testCase "if a then b else c : ??" $ assertEqual ""
            (Left "type error") -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if a then b else c")
        , testCase "(\\x:Bool->Bool.x) (\\x:Bool.x) : Bool -> Bool" $ assertEqual ""
            (Right $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "(\\x:Bool->Bool.x) (\\x:Bool.x)")
        , testCase "if true then (\\x:Bool.x) else (\\x:Bool.x) : Bool -> Bool" $ assertEqual ""
            (Right $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if true then (\\x:Bool.x) else (\\x:Bool.x)")
        , testCase "f:Bool->Bool ⊢ f (if false then true else false) : Bool" $ assertEqual ""
            (Just TypedLambda.TyBool) -- expected
            (TypedLambda.typeof' [("f", TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool)]
              =<< eitherToMaybe (TypedLambda.parse "f (if false then true else false)"))
        , testCase "f:Bool->Bool ⊢ \\x:Bool. f (if x then false else x) : Bool->Bool" $ assertEqual ""
            (Just $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof' [("f", TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool)]
              =<< eitherToMaybe (TypedLambda.parse "\\x:Bool. f (if x then false else x)"))
        , testCase "if true then (\\x:Bool.x) else (\\x:Bool->Bool.x) (\\x:Bool.x) : Bool -> Bool" $ assertEqual ""
            (Right $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if true then (\\x:Bool.x) else (\\x:Bool->Bool.x) (\\x:Bool.x)")
      ]
  ]

arithTest :: TestTree
arithTest = testGroup "Arith" [
      testProperty "parse is left inverse of unparse" $
        is_left_inverse_of ArithGen.genTerm Arith.parse Arith.unparse
    , testCase "if iszero succ 0 then false else true -->* true" $ assertEqual ""
        (Just "true") -- expected
        ((fmap (Arith.unparse . eval) . Arith.parse) "if iszero succ 0 then false else true")
    , testCase "if iszero 0 then 0 else pred 0 : Nat" $ assertEqual ""
        (Just TypedArith.TyNat) -- expected
        (TypedArith.typeof =<< Arith.parse "if iszero 0 then 0 else pred 0")
    , testCase "if true then 0 else false : ??" $ assertEqual ""
        (Nothing) -- expected
        (TypedArith.typeof =<< Arith.parse "if true then 0 else false")
  ]

expressionTutorTest :: TestTree
expressionTutorTest = testGroup "Expressiontutor" [
    testGroup "Untyped Lambda" [
        testProperty "nmToLang is left inverse of langToNm" $
          is_left_inverse_of genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> ExpTreeDiagram)
      , testProperty "commutation proof" $
          bisimulationCommutes genExp LambdaET.bisim
    ],
    testGroup "Arith" [
        testProperty "nmToLang is left inverse of langToNm" $
          is_left_inverse_of ArithGen.genTerm Inj.fromNM (Inj.toNM :: Arith.Term -> ExpTreeDiagram)
      , testProperty "commutation proof" $
          bisimulationCommutes ArithGen.genTerm ArithET.bisim
    ]
  ]

expTreeTest :: TestTree
expTreeTest = testGroup "Expression Trees" [
      testProperty "nmToLang is inverse of langToNm" $
        is_equivalent_to genExp (Bij.fromNM . Bij.toNM) id
    , testProperty "commutation proof" $
        bisimulationCommutes genExp ETree.bisim
  ]

reductTest :: TestTree
reductTest = testGroup "Reduct" [
      testProperty "nmToLang is left inverse of langToNm" $
        is_left_inverse_of genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> ReductExp)
    , testProperty "commutation proof" $
        bisimulationCommutes genExp R.bisim
    , testProperty "reduct trees have unique ids"
        prop_uniqids
  ]

alligatorTest :: TestTree
alligatorTest = testGroup "Alligators" [
      testProperty "nmToLang is left inverse of langToNm" $
        is_left_inverse_of genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> AlligatorFamilies)
    , testProperty "commutation proof" $
        bisimulationCommutes genExp A.bisim
    , testProperty "color rule"
        color_rule
    , testProperty "In example, right guess <=> right colors"
        game_play_example
    , testGroup "de Bruijn Alligators" (
      let f = fmap Lambda.unparse . A.nmToLang . deBruijnAlligators . Inj.toNM . fromJust . Lambda.parse
      in [
          testCase "id" $ assertEqual ""
            [HungryAlligator 0 [Egg 0]] -- expected
            (deBruijnAlligators [HungryAlligator (Color "a") [Egg (Color "a")]])
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
       is_equivalent_to genExp (show . toAscii . (Inj.toNM :: Lambda.Exp -> AlligatorFamilies)) (show . toAscii)
  ]

tests :: TestTree
tests = testGroup "Tests" [lambdaTest, arithTest, typLambdaTest, expressionTutorTest, expTreeTest, reductTest, alligatorTest]

defaultNumberOfTests = 300

main = Test.Tasty.defaultMain tests

