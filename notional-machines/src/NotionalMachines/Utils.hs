{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Utils where

import Data.List (uncons)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (forM_)

import Text.Pretty.Simple (pPrintOpt, CheckColorTty(..), defaultOutputOptionsDarkBg, outputOptionsCompact)
import Data.Text.Prettyprint.Doc (Pretty, pretty)


maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither l = maybe (Left l) Right

pShow :: Pretty a => a -> String
pShow = show . pretty

shortPrint :: Show a => a -> IO ()
shortPrint = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = False}

------- Generators utils ----------

genName :: MonadGen m => m String
genName = Gen.list (Range.singleton 1) $ Gen.element ['a'..'z']

sample :: Gen a -> IO a
sample = Gen.sample

printSample :: Show a => Int -> Gen a -> IO ()
printSample n gen = forM_ [1..n] (\_ -> shortPrint =<< Gen.sample gen)

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO b
genAndSolve gen solver =
            do e <- gen
               shortPrint e
               return $ solver e

