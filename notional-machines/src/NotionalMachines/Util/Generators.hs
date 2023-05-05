{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Util.Generators where

import Control.Monad (forM)

import           Hedgehog       (Gen, MonadGen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import NotionalMachines.Util.Util (shortPrint)


genName :: MonadGen m => m String
genName = Gen.list (Range.singleton 1) $ Gen.element ['a'..'z']

sample :: Gen a -> IO a
sample = Gen.sample

sampleN :: Show a => Int -> Gen a -> IO [a]
sampleN n gen = forM [1..n] (const $ Gen.sample gen)

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO b
genAndSolve gen solver =
            do e <- gen
               shortPrint e
               return $ solver e

