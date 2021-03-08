{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad

import Lib

genVariable :: Gen Variable
genVariable = do
   value  <- Gen.int (Range.linear 0 100)
   name <- Gen.list (Range.linear 0 100) Gen.alpha
   return $ Variable name value

prop_commutation :: Property
prop_commutation = property $ do
  v <- forAll genVariable
  (alpha_B . f') v === (f . alpha_A) v


tests :: IO Bool
tests =
  checkParallel $ Group "Test.VariableAsABox" [
        ("prop_commutation", prop_commutation)
    ]

main :: IO ()
-- main = forM_ [0..10] (\_ -> print =<< Gen.sample genVariable)
main = void tests
