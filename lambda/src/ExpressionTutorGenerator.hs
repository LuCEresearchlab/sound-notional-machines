{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}

module ExpressionTutorGenerator where

import           Hedgehog hiding (Var, eval, evalMaybe)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)

import Data.Functor.Identity (Identity)

import Text.Show.Pretty (pPrint, ppDoc)
import Text.PrettyPrint (Doc)

import UntypedLambda
import ExpressionTutor

----------------------
----- Generators -----
----------------------

----- Lambda -----

genName :: MonadGen m => m String
genName = Gen.list (Range.singleton 1) $ Gen.element ['a'..'z']

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

------------------
-- Expression Tutor activities
------------------

---- Parse activity ----

generateParseActivity :: MonadIO m => m String
generateParseActivity = unparse <$> Gen.sample genExp

solveParseActivity :: String -> Maybe ExpTreeDiagram
solveParseActivity = fmap lang2nm . parse


---- Unparse activity ----

generateUnparseActivity :: MonadIO m => m ExpTreeDiagram
generateUnparseActivity = lang2nm <$> Gen.sample genExp

solveUnparseActivity :: ExpTreeDiagram -> Maybe String
solveUnparseActivity = fmap unparse . nm2lang


---- Eval activity ----

generateEvalActivity :: MonadIO m => m String
generateEvalActivity = unparse <$> Gen.sample genCombinator

solveEvalActivity :: String -> Maybe String
solveEvalActivity = fmap unparse . (=<<) evalMaybe . parse


------- Utils ----------

sample :: Show a => Int -> Gen a -> IO ()
sample n gen = forM_ [1..n] (\_ -> pPrint =<< Gen.sample gen)

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO Doc
genAndSolve gen solver =
  ppDoc <$> do e <- gen
               pPrint e
               return $ solver e

