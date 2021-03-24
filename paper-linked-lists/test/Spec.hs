{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import           Hedgehog hiding (Var, eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (void) --, forM_)
-- import Control.Monad.IO.Class (MonadIO)

-- import Data.List ((\\))
-- import Data.Functor.Identity (Identity)

-- import Text.Show.Pretty (pPrint, ppDoc)
-- import Text.PrettyPrint (Doc)

import Lib

genElem :: MonadGen m => m String
genElem = Gen.list (Range.linear 0 10) $ Gen.alpha

genList :: MonadGen m => m [String]
genList = Gen.list (Range.linear 0 10) $ genElem


prop_inverses :: Property
prop_inverses = property $ do
  l <- forAll genList
  tripping l list2dia dia2list

prop_commutation :: Property
prop_commutation = property $ do
  l <- forAll genList
  (alphaB . f') l === (f . alphaA) l


tests :: IO Bool
tests =
  checkParallel $ Group "Test.ExpressionTree" [
        ("list2dia is the inverse of dia2list", prop_inverses)
      , ("commutation proof:", prop_commutation)
    ]


main :: IO ()
main = void tests
-- main = defaultMain [tests]




------- Ganerate activities ----------

-- sample :: Show a => Int -> Gen a -> IO ()
-- sample n gen = forM_ [0..n] (\_ -> pPrint =<< Gen.sample gen)

-- generateParseActivity :: MonadIO m => m ExpressionEnv
-- generateParseActivity = unparse . initProgram <$> Gen.sample genExp

-- generateUnparseActivity :: MonadIO m => m ExpTreeDiagram
-- generateUnparseActivity = ast2graph . initProgram <$> Gen.sample genExp

-- generateEvalActivity :: MonadIO m => m ExpressionEnv
-- generateEvalActivity = unparse . initProgram <$> Gen.sample genCombinator

-- genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO Doc
-- genAndSolve gen solver =
--   ppDoc <$> do e <- gen
--                pPrint e
--                return $ solver e

