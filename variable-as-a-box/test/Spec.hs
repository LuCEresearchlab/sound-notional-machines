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

prop_commutation_update :: Property
prop_commutation_update = property $ do
  v <- forAll genVariable
  value  <- forAll $ Gen.int (Range.linear 0 100)
  (alpha_A . updateVariable value) v === (updateBox value . alpha_A) v


tests :: IO Bool
tests =
  checkParallel $ Group "Test.VariableAsABox" [
        ("prop_commutation", prop_commutation)
      , ("prop_commutation_update", prop_commutation_update)
    ]

main :: IO ()
-- main = forM_ [0..10] (\_ -> print =<< Gen.sample genVariable)
main = void tests
