module NotionalMachines.Lang.List.Generators where

import           Hedgehog     (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import NotionalMachines.Lang.List.Main (List (Empty, Cons), haskellListToList)


genNilInput :: MonadGen m => m ()
genNilInput = Gen.constant ()

genConsInput :: MonadGen m => m (Char, List Char)
genConsInput = (,) <$> Gen.alpha <*> genList

genUnconsInput :: MonadGen m => m (List Char)
genUnconsInput = genList

genList :: MonadGen m => m (List Char)
genList = haskellListToList <$> Gen.list (Range.linear 0 100) Gen.alpha
