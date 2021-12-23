{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TupleSections     #-}

import           Hedgehog       hiding (Var, eval, evalM)
import qualified Hedgehog       (eval)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import           Data.Either   (isRight)
import           Data.Foldable (toList)
import           Data.List     (intersect)
import           Data.Maybe    (fromJust)
import qualified Data.Set      as Set
import Data.Function ((&))

import Control.Monad ((<=<), liftM2)
import Control.Monad.State.Lazy (runStateT)

import qualified NotionalMachines.Lang.TypedArith.Main                    as TypedArith
import qualified NotionalMachines.Lang.TypedLambdaArith.Main              as TypedLambda
import qualified NotionalMachines.Lang.UntypedArith.Generators            as ArithGen
import qualified NotionalMachines.Lang.UntypedArith.Main                  as Arith
import           NotionalMachines.Lang.UntypedLambda.AsciiAlligatorSyntax ()
import qualified NotionalMachines.Lang.UntypedLambda.Generators           as LambdaGen
import qualified NotionalMachines.Lang.UntypedLambda.Main                 as Lambda
-- import qualified NotionalMachines.Lang.TypedLambdaExpressionTutor as LambdaET
import qualified NotionalMachines.Lang.TypedLambdaArith.Generators   as TypedLambdaGen
import qualified NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax as LambdaRefAST
import qualified NotionalMachines.Lang.TypedLambdaRef.Main           as LambdaRef
-- import qualified NotionalMachines.Lang.TypedLambdaRefExpressionTutor as LambdaRefET
import qualified NotionalMachines.Lang.TypedLambdaRef.Generators as LambdaRefGen

import           NotionalMachines.Machine.AlligatorEggs.AsciiSyntax
import           NotionalMachines.Machine.AlligatorEggs.Main
import           NotionalMachines.Machine.ExpressionTutor.Generators (genExpTutorDiagram)
import           NotionalMachines.Machine.ExpressionTutor.Main       (Edge (..),
                                                                      ExpTutorDiagram (..),
                                                                      Node (..),
                                                                      NodeContentElem (..),
                                                                      Plug (..))
import           NotionalMachines.Machine.Reduct.Main                (ReductExp, ReductExpF (..),
                                                                      updateUids)
import qualified NotionalMachines.Machine.TAPLMemoryDiagram.Main     as TAPLMemDia

import qualified NotionalMachines.LangInMachine.TypedArithExpressionTutor       as TypedArithET (TyExpTutorDiagram,
                                                                                                 annotateTypeBisim,
                                                                                                 evalBisim,
                                                                                                 typeOfBisim)
import qualified NotionalMachines.LangInMachine.TypedLambdaRefTAPLMemoryDiagram as LambdaRefTAPLDia
import qualified NotionalMachines.LangInMachine.UntypedArithExpressionTutor     as ArithET (bisim)
import qualified NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs      as A (bisim,
                                                                                      langToNm)
import qualified NotionalMachines.LangInMachine.UntypedLambdaExpressionTree     as ETree (bisim)
import qualified NotionalMachines.LangInMachine.UntypedLambdaExpressionTutor    as LambdaET (bisim)
import qualified NotionalMachines.LangInMachine.UntypedLambdaReduct             as R (bisim)

import qualified NotionalMachines.Meta.Bijective    as Bij
import           NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import qualified NotionalMachines.Meta.Injective    as Inj
import           NotionalMachines.Meta.Steppable    (eval, evalM)

import NotionalMachines.Utils
import qualified Data.Map as Map

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
genColor = nameToColor <$> Gen.list (Range.singleton 1) (Gen.element ['a'..'z'])


---------------------------
----- Meta Properties -----
---------------------------

prop :: PropertyT IO () -> Property
prop = withTests defaultNumberOfTests . property

----- Lambda -----

evalProducesValue :: Property
evalProducesValue = prop $ do
  e <- forAll LambdaGen.genCombinator
  (Lambda.isValue <$> evalM e) === Just True

typeSafety :: (Show term, Show typ, Show er, Eq er)
            => Gen term -> (term -> Either er typ) -> (term -> Either er term) -> (term -> Bool) -> Property
typeSafety g typer evaluer isValuer = prop $ do
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
  (not a || b) === True

isLeftInverseOf :: (Applicative f, Show (f b), Eq (f b), Show a, Show b)
                   => Gen b -> (a -> f b) -> (b -> a) -> Property
isLeftInverseOf g f f' = prop $ do
  e <- forAll g
  -- classify "closed terms" $ null (freeVs e)
  -- classify "open terms" $ not (null (freeVs e))
  -- classify "depth 0" $ depth e == 0
  -- classify "depth <= 2" $ depth e <= 2
  -- classify "depth > 7" $ depth e > 7
  tripping e f' f

isEquivalentTo :: (Eq a, Show a, Show e) => Gen e -> (e -> a) -> (e -> a) -> Property
isEquivalentTo g f f' = prop $ do
  e <- forAll g
  f e === f' e

bisimulationCommutes :: (Eq b, Show b, Show a') => Gen a' -> Bisimulation a' b' a b -> Property
bisimulationCommutes g b = isEquivalentTo g (alphaB b . fLang b) (fNM b . alphaA b)

----- Reduct -----

prop_uniqids :: Property
prop_uniqids = prop $ do
  e <- forAll genReductExp
  let ids = uids $ updateUids 0 e
  ids === [0..(length ids - 1)]

uids :: ReductExpF a -> [a]
uids = toList

----- Alligator -----

colorRuleProp :: Property
colorRuleProp = prop $ do
  e1 <- forAll LambdaGen.genExp
  case alphaA A.bisim e1 of
    a1:a2:_ ->
      let newA2 = recolor a1 a2
      in do annotateShow a1
            annotateShow a2
            annotateShow newA2
            deBruijnAlligators [newA2] === deBruijnAlligators [a2]
            null (toList a1 `intersect` toList newA2) === True
    _ -> success

gamePlayExample :: Property
gamePlayExample = prop $ do
  c1 <- forAll genColor
  c2 <- forAll genColor
  let rightGuess = guess [anot] [([atru], [afls]), ([afls], [atru])] [c1, c2]
  let rightColors = [c1, c2] == [nameToColor "c", nameToColor "b"]
  (rightColors && rightGuess || not rightColors && not rightGuess) === True

---------------------------
----- Meta Test Cases -----
---------------------------

evalTo :: (Monad m, Eq (m String), Show (m String))
        => String -> String -> (String -> m String) -> TestTree
evalTo input output f =
  testCase (input ++ " -->* " ++ output) $ assertEqual ""
    (return output) -- expected
    (f input)

-----------------
----- Tests -----
-----------------

arithTest :: TestTree
arithTest = testGroup "Untyped and typed arith" [
      testProperty "parse is left inverse of unparse" $
        isLeftInverseOf ArithGen.genTerm Arith.parse Arith.unparse
    , evalTo "if iszero succ 0 then false else true" "true"
        (fmap (Arith.unparse . eval) . Arith.parse)
    , testCase "if iszero 0 then 0 else pred 0 : Nat" $ assertEqual ""
        (Right TypedArith.TyNat) -- expected
        (TypedArith.typeof =<< TypedArith.parse "if iszero 0 then 0 else pred 0")
    , testCase "if true then 0 else false : ??" $ assertEqual ""
        (Left (TypedArith.TypeError "expected 'Nat' but 'false' has type 'Bool' in expression 'if true then 0 else false'.")) -- expected
        (TypedArith.typeof =<< TypedArith.parse "if true then 0 else false")
    , evalTo "if iszero 0 then succ 0 else false" "succ 0"
        Arith.replEval
    , evalTo "if iszero succ 0 then succ 0 else false" "false"
        Arith.replEval
    , evalTo "if (iszero (succ 0)) then succ 0 else false" "false"
        Arith.replEval
    , evalTo "if (iszero (succ 0)) then (succ 0) else false" "false"
        Arith.replEval
    , evalTo "if false then succ 0 else false" "false"
        Arith.replEval
    , evalTo "(0)" "0"
        Arith.replEval
    , evalTo "(false)" "false"
        Arith.replEval
    , evalTo "(succ 0)" "succ 0"
        Arith.replEval
    , evalTo "succ (succ 0)" "succ succ 0"
        Arith.replEval
    , evalTo "succ (succ (succ 0))" "succ succ succ 0"
        Arith.replEval
  ]

lambdaTest :: TestTree
lambdaTest = testGroup "Untyped Lambda Calculus" [
      testProperty "eval produces a value"
        evalProducesValue
    , testProperty "parse is left inverse of unparse" $
        isLeftInverseOf LambdaGen.genExp Lambda.parse Lambda.unparse
    , evalTo "(((x)))" "x"
        Lambda.replEval
    -- TAPL p. 59
    , evalTo "(\\l.\\m.\\n.l m n) (\\t.\\f.t) v w" "v"
        Lambda.replEval
    , evalTo "(\\x. ((\\z. (\\zz. zz)) x) ((\\x. (\\w. (\\y. y)) x) t)      x) a" "a"
        Lambda.replEval
  ]

typLambdaTest :: TestTree
typLambdaTest = testGroup "Typed Lambda Calculus" [
      testProperty "parse is left inverse of unparse" $
        isLeftInverseOf TypedLambdaGen.genTerm TypedLambda.parse TypedLambda.unparse
    , testProperty "language is type safe" $
        typeSafety TypedLambdaGen.genTerm TypedLambda.typeof (return . eval) TypedLambda.isValue
    , testGroup "Type checking" [
          testCase "if iszero 0 then 0 else pred 0 : Nat" $ assertEqual ""
            (Right TypedLambda.TyNat) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if iszero 0 then 0 else pred 0")
        , testCase "if true then 0 else false : ??" $ assertEqual ""
            (Left (TypedLambda.TypeError "expected 'Nat' but 'false' has type 'Bool' in expression 'if true then 0 else false'.")) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if true then 0 else false")
        , testCase "if a then b else c : ??" $ assertEqual ""
            (Left (TypedLambda.TypeError "variable 'a' not in scope.")) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if a then b else c")
        , testCase "(\\x:Bool->Bool.x) (\\x:Bool.x) : Bool -> Bool" $ assertEqual ""
            (Right $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "(\\x:Bool->Bool.x) (\\x:Bool.x)")
        , testCase "if true then (\\x:Bool.x) else (\\x:Bool.x) : Bool -> Bool" $ assertEqual ""
            (Right $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if true then (\\x:Bool.x) else (\\x:Bool.x)")
        , testCase "f:Bool->Bool ⊢ f (if false then true else false) : Bool" $ assertEqual ""
            (Right TypedLambda.TyBool) -- expected
            (TypedLambda.typeof' [("f", TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool)]
              =<< TypedLambda.parse "f (if false then true else false)")
        , testCase "f:Bool->Bool ⊢ \\x:Bool. f (if x then false else x) : Bool->Bool" $ assertEqual ""
            (Right $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof' [("f", TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool)]
              =<< TypedLambda.parse "\\x:Bool. f (if x then false else x)")
        , testCase "if true then (\\x:Bool.x) else (\\x:Bool->Bool.x) (\\x:Bool.x) : Bool -> Bool" $ assertEqual ""
            (Right $ TypedLambda.TyFun TypedLambda.TyBool TypedLambda.TyBool) -- expected
            (TypedLambda.typeof =<< TypedLambda.parse "if true then (\\x:Bool.x) else (\\x:Bool->Bool.x) (\\x:Bool.x)")
        , evalTo "(\\x:Nat. ((\\z:Nat. (\\zz:Nat->Nat. zz)) x) ((\\x:Bool. (\\w:Bool. (\\y:Nat. y)) x) true)      x) (succ 0)" "succ (0) : Nat"
            TypedLambda.replEval
      ]
  ]

typLambdaRefTest :: TestTree
typLambdaRefTest = testGroup "Typed Lambda Ref" [
      testProperty "parse is left inverse of unparse" $
        isLeftInverseOf LambdaRefGen.genTerm LambdaRef.parse LambdaRef.unparse
    , testProperty "language is type safe" $
        typeSafety LambdaRefGen.genTerm LambdaRef.typeof LambdaRef.evalM' LambdaRef.isValue
    , testGroup "Parsing" [
          testCase "parse and unparse 'r:=succ(!r); r:=succ(!r); !r'" $ assertEqual ""
            (Right "r := succ (!r); r := succ (!r); !r") -- expected
            (LambdaRef.unparse <$> LambdaRef.parse "r:=succ(!r); r:=succ(!r); !r")
    ]
    , testGroup "Evaluation" [
          evalTo "if iszero (pred 2) then 0 else (if iszero (pred 0) then succ 2 else 0)" "3 : Nat"
            LambdaRef.replEval


        -- Example that uses variable shadowing and variable captured by closure:
        , evalTo "(\\x:Nat. ((\\z:Nat. (\\zz:Nat->Nat. zz)) x) ((\\x:Bool. (\\w:Bool. (\\y:Nat. y)) x) true) x) 1" "1 : Nat"
            LambdaRef.replEval
        -- Example where environment contains closure that contains an environment with another closure (that contains an environment):
        , evalTo "(\\zz:Nat->Nat. (\\xx:Nat->Nat->Nat. xx 1 2) (\\y:Nat. zz)) (\\z:Nat. z)" "2 : Nat"
            LambdaRef.replEval


        , evalTo "(\\r:Ref Nat. if false then (r := 82; !r) else (!r)) (ref 13)" "13 : Nat"
            LambdaRef.replEval
        --- Incrementing a variable
        , evalTo "(\\r:Ref Nat. r:=succ(!r); r:=succ(!r); !r) (ref 0)" "2 : Nat"
            LambdaRef.replEval

        -- Set 'r' to 13 and read from it
        , evalTo "(\\r:Ref Nat.(\\s:Ref Nat.          !r) r) (ref 13)" "13 : Nat"
            LambdaRef.replEval
        -- Same program as before, the only difference is that here we're also
        -- setting 's' to 82.  But wait... if we never changed the value of
        -- 'r', how can it now be 82? How can changing another variable (a
        -- variable we're not using to produce the result) affect the result of
        -- the program? If wee're not using 's' to produce the result of the
        -- program, how can changing it's value affect the result of the
        -- program? what does 's' have anything to do with 'r'? (TAPL p.156)
        , evalTo "(\\r:Ref Nat.(\\s:Ref Nat. s := 82; !r) r) (ref 13)" "82 : Nat"
            LambdaRef.replEval

        -- Both 's' and 'r' are set to 2. If we then set 'r' to 0 and then set
        -- 'r' to the value of 's' this is equivalento to not setting 'r' to 0
        -- in the first place (as shown in the next two tests). One should be
        -- able to always remove this first mutation, right? Well... not if 's'
        -- and 'r' refer to the same cell (see the third test). (TAPL p.156)
        , evalTo "(\\r:Ref Nat.(\\s:Ref Nat. r := 0; r := !s; !r)) (ref 2) (ref 2)" "2 : Nat"
            LambdaRef.replEval
        , evalTo "(\\r:Ref Nat.(\\s:Ref Nat.         r := !s; !r)) (ref 2) (ref 2)" "2 : Nat"
            LambdaRef.replEval
        , evalTo "(\\x:Ref Nat.(\\r:Ref Nat.(\\s:Ref Nat.r := 0; r := !s; !r)) x x) (ref 2)" "0 : Nat"
            LambdaRef.replEval

    ]
    , testGroup "LambdaRef ala Wadler" [
        -- Example that uses variable shadowing and variable captured by closure:
          evalTo "(\\x:Nat. ((\\z:Nat. (\\zz:Nat->Nat. zz)) x) ((\\x:Bool. (\\w:Bool. (\\y:Nat. y)) x) true) x) 1" "1 : Nat"
            LambdaRef.replEvalAlaWadler
        -- Example where environment contains closure that contains an environment with another closure (that contains an environment):
        , evalTo "(\\zz:Nat->Nat. (\\xx:Nat->Nat->Nat. xx 1 2) (\\y:Nat. zz)) (\\z:Nat. z)" "2 : Nat"
            LambdaRef.replEvalAlaWadler
    ]
    , testGroup "LambdaRef ala Racket" [
        -- Example that uses variable shadowing and variable captured by closure:
          evalTo "(\\x:Nat. ((\\z:Nat. (\\zz:Nat->Nat. zz)) x) ((\\x:Bool. (\\w:Bool. (\\y:Nat. y)) x) true) x) 1" "1 : Nat"
            LambdaRef.replEvalAlaRacket
        -- Example where environment contains closure that contains an environment with another closure (that contains an environment):
        , evalTo "(\\zz:Nat->Nat. (\\xx:Nat->Nat->Nat. xx 1 2) (\\y:Nat. zz)) (\\z:Nat. z)" "2 : Nat"
            LambdaRef.replEvalAlaRacket
        -- Example with fresh name inside lambda inside name env
        , evalTo "(\\x:Nat->Nat. (\\x:Nat->Nat->Nat. (\\z:Nat->Nat->Nat->Nat. z 9) (\\w:Nat. x) 0 1) (\\y:Nat. x)) (\\x:Nat. x)" "1 : Nat"
            LambdaRef.replEvalAlaRacket
        -- Example with store cell that refers to store cell
        , evalTo "ref (ref 1)" "Loc 1 : Ref (Ref Nat)"
            LambdaRef.replEvalAlaRacket
    ]
    , testGroup "Store" [
          testCase "deref on empty store" $ assertEqual ""
            (Left (LambdaRefAST.RuntimeError "address not found: 0")) -- expected
            (runStateT (LambdaRefAST.deref 0) Map.empty)
        , testCase "assign on empty store" $ assertEqual ""
            (Left (LambdaRefAST.RuntimeError "address not found: 0")) -- expected
            (runStateT (LambdaRefAST.assign 0 LambdaRef.Zero) Map.empty)
        , testProperty "`(deref <=< alloc) t === t` (modulo glue code)" $
            isEquivalentTo LambdaRefGen.genTermStore
                           (return . fst)
                           (fmap fst . stateToTuple (LambdaRefAST.deref <=< stateToStateT . LambdaRefAST.alloc))
    ]
  ]

expressionTutorTest :: TestTree
expressionTutorTest = testGroup "Expressiontutor" [
    testGroup "ET with Untyped Lambda" [
        testProperty "nmToLang is left inverse of langToNm" $
          isLeftInverseOf LambdaGen.genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> ExpTutorDiagram)
      , testProperty "commutation proof" $
          bisimulationCommutes LambdaGen.genExp LambdaET.bisim
    ],
    testGroup "ET with Arith" [
        testProperty "nmToLang is left inverse of langToNm" $
          isLeftInverseOf ArithGen.genTerm Inj.fromNM (Inj.toNM :: Arith.Term -> ExpTutorDiagram)
      , testProperty "commutation proof" $
          bisimulationCommutes ArithGen.genTerm ArithET.bisim
    ],
    testGroup "ET with Typed Arith" [
        testProperty "nmToLang is left inverse of langToNm" $
          isLeftInverseOf ArithGen.genTerm Inj.fromNM (Inj.toNM :: Arith.Term -> TypedArithET.TyExpTutorDiagram)
      , testProperty "commutation proof for evaluation bisimulation" $
          bisimulationCommutes ArithGen.genTerm TypedArithET.evalBisim
      , testProperty "commutation proof for typeof bisim (ask for type of term)" $
          bisimulationCommutes ArithGen.genTerm TypedArithET.typeOfBisim
      , testProperty "commutation proof for type annotated diagram" $
          bisimulationCommutes ArithGen.genTerm TypedArithET.annotateTypeBisim
    ],
    testGroup "Malformed ETs" [
        testProperty "Random diagram doesn't crash" $ prop $
          do d <- forAll genExpTutorDiagram
             _ <- Hedgehog.eval (Inj.fromNM d :: Maybe Lambda.Exp)
             success
      , testCase "Diagram with cycles to NM terminates" $ assertEqual ""
          Nothing -- expected
          (Inj.fromNM $ ExpTutorDiagram {
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
        isEquivalentTo LambdaGen.genExp (Bij.fromNM . Bij.toNM) id
    , testProperty "commutation proof" $
        bisimulationCommutes LambdaGen.genExp ETree.bisim
  ]

reductTest :: TestTree
reductTest = testGroup "Reduct" [
      testProperty "nmToLang is left inverse of langToNm" $
        isLeftInverseOf LambdaGen.genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> ReductExp)
    , testProperty "commutation proof" $
        bisimulationCommutes LambdaGen.genExp R.bisim
    , testProperty "reduct trees have unique ids"
        prop_uniqids
  ]

alligatorTest :: TestTree
alligatorTest = testGroup "Alligators" [
      -- testProperty "nmToLang is left inverse of langToNm" $
      --   isLeftInverseOf LambdaGen.genExp Inj.fromNM (Inj.toNM :: Lambda.Exp -> AlligatorFamilies)
      testProperty "commutation proof" $
        bisimulationCommutes LambdaGen.genCombinator A.bisim
    , testProperty "color rule"
        colorRuleProp
    , testProperty "In example, right guess <=> right colors"
        gamePlayExample
    , testGroup "de Bruijn Alligators" (
      let f = fmap Lambda.unparse . (=<<) (nmToLang' show . deBruijnAlligators . A.langToNm) . eitherToMaybe . Lambda.parse
      in [
          testCase "id" $ assertEqual ""
            [HungryAlligator 0 [Egg 0]] -- expected
            (deBruijnAlligators [HungryAlligator (nameToColor "a") [Egg (nameToColor "a")]])
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
       isEquivalentTo LambdaGen.genExp (show . toAscii . A.langToNm) (show . toAscii)
  ]
    where
      nmToLang' :: (a -> String) -> [AlligatorFamilyF a] -> Maybe Lambda.Exp
      nmToLang' toName families = fmap f2e families & \e -> case e of
          []           -> Nothing
          [me]         -> me
          me1:me2:rest -> foldl (liftM2 Lambda.App) (liftM2 Lambda.App me1 me2) rest
        where f2e (HungryAlligator c proteges) = Lambda.Lambda (toName c) <$> nmToLang' toName proteges
              f2e (OldAlligator proteges)      = nmToLang' toName proteges
              f2e (Egg c)                      = Just (Lambda.Var (toName c))


taplMemeryDiagramTest :: TestTree
taplMemeryDiagramTest = testGroup "TAPL Memory Diagram" [
      testProperty "`(deref . alloc) t === return t` (modulo glue code)" $
        isEquivalentTo LambdaRefGen.genTerm
                       (return)
                       (TAPLMemDia.tDeref . TAPLMemDia.tAlloc LambdaRefAST.nextLocation . (, TAPLMemDia.emptyDiagram))
    , testProperty "commutation proof for alloc" $
        bisimulationCommutes LambdaRefGen.genTermStore LambdaRefTAPLDia.allocBisim
    , testProperty "commutation proof for deref" $
        bisimulationCommutes LambdaRefGen.genLocationStore LambdaRefTAPLDia.derefBisim
    , testProperty "commutation proof for assign" $
        bisimulationCommutes LambdaRefGen.genLocationTermStore LambdaRefTAPLDia.assignBisim
  ]

tests :: TestTree
tests = testGroup "Tests" [
            testGroup "Languages"         [lambdaTest, arithTest, typLambdaTest, typLambdaRefTest]
          , testGroup "Notional Machines" [expressionTutorTest, expTreeTest, reductTest, alligatorTest, taplMemeryDiagramTest]
        ]

defaultNumberOfTests :: TestLimit
defaultNumberOfTests = 300

main :: IO ()
main = Test.Tasty.defaultMain tests

