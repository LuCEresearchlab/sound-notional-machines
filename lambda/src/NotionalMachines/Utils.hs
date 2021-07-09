{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Utils where

import Data.List (uncons)

maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

