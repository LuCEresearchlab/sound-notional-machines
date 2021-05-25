{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import           Hedgehog hiding (Var, eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Main (defaultMain)

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)

import Data.Functor.Identity (Identity)

import Text.Show.Pretty (pPrint, ppDoc)
import Text.PrettyPrint (Doc)

import Lib

genName :: MonadGen m => m String
genName = Gen.list (Range.singleton 1) $ Gen.element ['a'..'c']

genExp :: MonadGen m => m Exp
genExp =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> genName
    ] [
      -- recursive generators
      Gen.subtermM genExp (\x -> Lambda <$> genName <*> pure x)
    , Gen.subterm2 genExp genExp App
    ]

genCombinator :: (MonadGen m, GenBase m ~ Identity) => m Exp
genCombinator = fmap bindFreeVars genExp

bindFreeVars :: Exp -> Exp
bindFreeVars e = foldl (\en var -> App (Lambda var en) (Lambda "a" (Var "a"))) e (freeVs e)


prop_eval_to_value :: Property
prop_eval_to_value = property $ do
  e <- forAll genCombinator
  (isValue <$> eval e) === Just True

isValue :: Exp -> Bool
isValue Lambda {} = True
isValue _         = False

prop_eval_equiv_step :: Property
prop_eval_equiv_step = property $ do
  e <- forAll genCombinator
  eval e === bigStep e

prop_parse_is_inverse_upparse :: Property
prop_parse_is_inverse_upparse = property $ do
  e <- forAll genExp
  tripping e unparse parse

prop_ast2diagram_is_inverse_diagram2ast :: Property
prop_ast2diagram_is_inverse_diagram2ast = property $ do
  e <- forAll genExp
  tripping e ast2diagram diagram2ast

prop_commutation :: Property
prop_commutation = property $ do
  e <- forAll genCombinator
  let expEnv = unparse e
  (alphaB . f') expEnv === (f . alphaA) expEnv


tests :: IO Bool
tests =
  checkParallel $ Group "Test.ExpressionTree" [
        ("eval returns a value:", prop_eval_to_value)
      , ("eval is equivalent to bigStep:", prop_eval_equiv_step)
      , ("parse is the inverse of unparse:", prop_parse_is_inverse_upparse)
      , ("ast2diagram is the inverse of diagram2ast:", prop_ast2diagram_is_inverse_diagram2ast)
      , ("commutation proof:", prop_commutation)
    ]


main :: IO ()
main = defaultMain [tests]




------- Ganerate activities ----------

sample :: Show a => Int -> Gen a -> IO ()
sample n gen = forM_ [1..n] (\_ -> pPrint =<< Gen.sample gen)

generateParseActivity :: MonadIO m => m String
generateParseActivity = unparse <$> Gen.sample genExp

generateUnparseActivity :: MonadIO m => m ExpTreeDiagram
generateUnparseActivity = ast2diagram <$> Gen.sample genExp

generateEvalActivity :: MonadIO m => m String
generateEvalActivity = unparse <$> Gen.sample genCombinator

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO Doc
genAndSolve gen solver =
  ppDoc <$> do e <- gen
               pPrint e
               return $ solver e

