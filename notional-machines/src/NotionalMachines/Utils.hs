{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Utils where

import Data.List (uncons)

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import Control.Monad (forM_)

import Text.Show.Pretty (pPrint, ppDoc)
import Text.PrettyPrint (Doc)

maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

------- Generators utils ----------

sample :: Gen a -> IO a
sample gen = Gen.sample gen

printSample :: Show a => Int -> Gen a -> IO ()
printSample n gen = forM_ [1..n] (\_ -> pPrint =<< Gen.sample gen)

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO Doc
genAndSolve gen solver =
  ppDoc <$> do e <- gen
               pPrint e
               return $ solver e

