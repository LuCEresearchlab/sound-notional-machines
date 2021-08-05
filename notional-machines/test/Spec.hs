{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

import           Hedgehog hiding (Var, eval, evalM)
import qualified Hedgehog (eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import Data.Foldable (toList)
import Data.List (intersect)
import Data.Maybe (fromJust)
import Data.Either (isRight)
import qualified Data.Set as Set


import qualified NotionalMachines.Lang.UntypedLambda.Main       as Lambda
import qualified NotionalMachines.Lang.UntypedLambda.Generators as LambdaGen
import           NotionalMachines.Lang.UntypedLambda.AsciiAlligatorSyntax ()
import qualified NotionalMachines.Lang.UntypedArith.Main       as Arith
import qualified NotionalMachines.Lang.UntypedArith.Generators as ArithGen
import qualified NotionalMachines.Lang.TypedArith.Main as TypedArith
import qualified NotionalMachines.Lang.TypedLambdaArith.Main       as TypedLambda
-- import qualified NotionalMachines.Lang.TypedLambdaExpressionTutor as LambdaET
import qualified NotionalMachines.Lang.TypedLambdaArith.Generators as TypedLambdaGen
import qualified NotionalMachines.Lang.TypedLambdaRef.Main       as TypedLambdaRef
-- import qualified NotionalMachines.Lang.TypedLambdaRefExpressionTutor as LambdaRefET
import qualified NotionalMachines.Lang.TypedLambdaRef.Generators as TypedLambdaRefGen

import           NotionalMachines.Machine.ExpressionTutor.Main (ExpTreeDiagram(..), NodeContentElem(..), Node(..), Plug(..), Edge(..))
import           NotionalMachines.Machine.ExpressionTutor.Generators (genExpTreeDiagram)
import           NotionalMachines.Machine.Reduct.Main (ReductExp, ReductExpF(..), updateUids)
import           NotionalMachines.Machine.AlligatorEggs.Main
import           NotionalMachines.Machine.AlligatorEggs.AsciiSyntax

import qualified NotionalMachines.LangInMachine.UntypedLambdaExpressionTree  as ETree        (bisim)
import qualified NotionalMachines.LangInMachine.UntypedLambdaReduct          as R            (bisim)
import qualified NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs   as A            (bisim, nmToLang)
import qualified NotionalMachines.LangInMachine.UntypedLambdaExpressionTutor as LambdaET     (bisim)
import qualified NotionalMachines.LangInMachine.UntypedArithExpressionTutor  as ArithET      (bisim)
import qualified NotionalMachines.LangInMachine.TypedArithExpressionTutor    as TypedArithET (evalBisim, typeOfBisim, annotateTypeBisim, TyExpTreeDiagram)

import           NotionalMachines.Meta.Bisimulation (Bisimulation(..))
import           NotionalMachines.Meta.Steppable (eval, evalM)
import qualified NotionalMachines.Meta.Injective as Inj
import qualified NotionalMachines.Meta.Bijective as Bij

import NotionalMachines.Utils (eitherToMaybe, genName)

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


---------------------------
----- Meta Properties -----
---------------------------

prop :: PropertyT IO () -> Property
prop = withTests defaultNumberOfTests . property

----- Lambda -----

eval_produces_value :: Property
eval_produces_value = prop $ do
  e <- forAll LambdaGen.genCombinator
  (Lambda.isValue <$> evalM e) === Just True

type_safety :: (Show term, Show typ)
            => Gen term -> (term -> Either String typ) -> (term -> Either String term) -> (term -> Bool) -> Property
type_safety g typer evaluer isValuer = prop $ do
  e <- forAll g
  classify "type checks" $ (isRight . typer) e
  classify "eval to val" $ Right True == (isValuer <$> evaluer e)
  annotateShow $ typer e
  annotateShow $ evaluer e
  annotateShow $ isValuer <$> evaluer e
  -- type-checks => evals to value  (i.e. doesn't get stuck)
  -- a => b == (not a) || b
  let a = (isRight . typer) e
  let b = (isValuer . fromJust . eitherToMaybe . evaluer) e
  ((not a) || b) === True

is_left_inverse_of :: (Applicative f, Show (f b), Eq (f b), Show a, Show b)
                   => Gen b -> (a -> f b) -> (b -> a) -> Property
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

uids :: ReductExpF a -> [a]
uids = toList

----- Alligator -----

color_rule :: Property
color_rule = prop $ do
  e1 <- forAll LambdaGen.genExp
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

---------------------------
----- Meta Test Cases -----
---------------------------

eval_to :: (Monad m, Eq (m String), Show (m String))
        => String -> String -> (String -> m String) -> TestTree
eval_to input output f = 
  testCase (input ++ " -->* " ++ output) $ assertEqual ""
    (return output) -- expected
    (f input)

-----------------
----- Tests -----
-----------------

lambdaTest :: TestTree
lambdaTest = testGroup "Untyped Lambda Calculus" [
      testProperty "eval produces a value"
        eval_produces_value
    , testProperty "parse is left inverse of unparse" $
        is_left_inverse_of LambdaGen.genExp Lambda.parse Lambda.unparse
  ]

typLambdaTest :: TestTree
typLambdaTest = testGroup "Typed Lambda Calculus" [
      testProperty "parse is left inverse of unparse" $
        is_left_inverse_of TypedLambdaGen.genTerm TypedLambda.parse TypedLambda.unparse
    , testProperty "language is type safe" $
        type_safety TypedLambdaGen.genTerm TypedLambda.typeof (return . eval) TypedLambda.isValue
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

typLambdaRefTest :: TestTree
typLambdaRefTest = testGroup "Typed Lambda Ref" [
      testProperty "parse is left inverse of unparse" $
        is_left_inverse_of TypedLambdaRefGen.genTerm TypedLambdaRef.parse TypedLambdaRef.unparse
    , testProperty "language is type safe" $
        type_safety TypedLambdaRefGen.genTerm TypedLambdaRef.typeof TypedLambdaRef.evalM' TypedLambdaRef.isValue
    , testGroup "Parsing" [
          testCase "parse and unparse 'r:=succ(!r); r:=succ(!r); !r'" $ assertEqual ""
            (Right $ "r := succ (!r); r := succ (!r); !r") -- expected
            (TypedLambdaRef.unparse <$> TypedLambdaRef.parse "r:=succ(!r); r:=succ(!r); !r")
    ]
    , testGroup "Evaluation" [
          eval_to "if iszero (pred 2) then 0 else (if iszero (pred 0) then succ 2 else 0)" "3 : Nat"
            (TypedLambdaRef.replEval)
        , eval_to "(\\r:Ref Nat. if false then (r := 82; !r) else (!r)) (ref 13)" "13 : Nat"
            (TypedLambdaRef.replEval)
        , eval_to "(\\r:Ref Nat. r:=succ(!r); r:=succ(!r); !r) (ref 0)" "2 : Nat"
            (TypedLambdaRef.replEval)
        , eval_to "(\\r:Ref Nat.(\\s:Ref Nat.          !r) r) (ref 13)" "13 : Nat"
            (TypedLambdaRef.replEval)
        , eval_to "(\\r:Ref Nat.(\\s:Ref Nat. s := 82; !r) r) (ref 13)" "82 : Nat"
            (TypedLambdaRef.replEval)
        , eval_to "(\\r:Ref Nat.(\\s:Ref Nat. r := 0; r := !s; !r)) (ref 2) (ref 2)" "2 : Nat"
            (TypedLambdaRef.replEval)
        , eval_to "(\\r:Ref Nat.(\\s:Ref Nat.         r := !s; !r)) (ref 2) (ref 2)" "2 : Nat"
            (TypedLambdaRef.replEval)
        , eval_to "(\\x:Ref Nat.(\\r:Ref Nat.(\\s:Ref Nat.r := 0; r := !s; !r)) x x) (ref 2)" "0 : Nat"
            (TypedLambdaRef.replEval)
    ]
  ]

arithTest :: TestTree
arithTest = testGroup "Arith" [
      testProperty "parse is left inverse of unparse" $
        is_left_inverse_of ArithGen.genTerm Arith.parse Arith.unparse
    , eval_to "if iszero succ 0 then false else true" "true"
        (fmap (Arith.unparse . eval) . Arith.parse)
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
          is_left_inverse_of LambdaGen.genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> ExpTreeDiagram)
      , testProperty "commutation proof" $
          bisimulationCommutes LambdaGen.genExp LambdaET.bisim
    ],
    testGroup "Arith" [
        testProperty "nmToLang is left inverse of langToNm" $
          is_left_inverse_of ArithGen.genTerm Inj.fromNM (Inj.toNM :: Arith.Term -> ExpTreeDiagram)
      , testProperty "commutation proof" $
          bisimulationCommutes ArithGen.genTerm ArithET.bisim
    ],
    testGroup "Typed Arith" [
        testProperty "nmToLang is left inverse of langToNm" $
          is_left_inverse_of ArithGen.genTerm Inj.fromNM (Inj.toNM :: Arith.Term -> TypedArithET.TyExpTreeDiagram)
      , testProperty "commutation proof for evaluation bisimulation" $
          bisimulationCommutes ArithGen.genTerm TypedArithET.evalBisim
      , testProperty "commutation proof for typeof bisim (ask for type of term)" $
          bisimulationCommutes ArithGen.genTerm TypedArithET.typeOfBisim
      , testProperty "commutation proof for type annotated diagram" $
          bisimulationCommutes ArithGen.genTerm TypedArithET.annotateTypeBisim
    ],
    testGroup "Malformed diagrams" [
        testProperty "Random diagram doesn't crash" $ prop $
          do d <- forAll genExpTreeDiagram
             _ <- Hedgehog.eval (Inj.fromNM d :: Maybe Lambda.Exp)
             success
      , testCase "Diagram with cycles to NM terminates" $ assertEqual ""
          (Nothing) -- expected
          (Inj.fromNM $ ExpTreeDiagram {
             nodes = Set.fromList [
               Node { nodePlug = Plug (0,0),
                      typ = Nothing,
                      content = [C "lambda", NameDef "x", Hole (Plug (0,1))] },
               Node { nodePlug = Plug (1,0),
                      typ = Nothing,
                      content = [C "lambda", NameDef "y", Hole (Plug (1,1))] }],
             edges = Set.fromList [Edge (Plug (0,1)) (Plug (1,0)),
                                   Edge (Plug (1,1)) (Plug (0,0))],
             root  = Just (Node {
               nodePlug = Plug (0,0),
               typ = Nothing,
               content = [C "lambda", NameDef "x", Hole (Plug (0,1))] }) } :: Maybe Lambda.Exp)
    ] 
  ]

expTreeTest :: TestTree
expTreeTest = testGroup "Expression Trees" [
      testProperty "nmToLang is inverse of langToNm" $
        is_equivalent_to LambdaGen.genExp (Bij.fromNM . Bij.toNM) id
    , testProperty "commutation proof" $
        bisimulationCommutes LambdaGen.genExp ETree.bisim
  ]

reductTest :: TestTree
reductTest = testGroup "Reduct" [
      testProperty "nmToLang is left inverse of langToNm" $
        is_left_inverse_of LambdaGen.genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> ReductExp)
    , testProperty "commutation proof" $
        bisimulationCommutes LambdaGen.genExp R.bisim
    , testProperty "reduct trees have unique ids"
        prop_uniqids
  ]

alligatorTest :: TestTree
alligatorTest = testGroup "Alligators" [
      testProperty "nmToLang is left inverse of langToNm" $
        is_left_inverse_of LambdaGen.genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> AlligatorFamilies)
    , testProperty "commutation proof" $
        bisimulationCommutes LambdaGen.genExp A.bisim
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
       is_equivalent_to LambdaGen.genExp (show . toAscii . (Inj.toNM :: Lambda.Exp -> AlligatorFamilies)) (show . toAscii)
  ]

tests :: TestTree
tests = testGroup "Tests" [lambdaTest, arithTest, typLambdaTest, typLambdaRefTest, expressionTutorTest, expTreeTest, reductTest, alligatorTest]

defaultNumberOfTests :: TestLimit
defaultNumberOfTests = 300

main :: IO ()
main = Test.Tasty.defaultMain tests

