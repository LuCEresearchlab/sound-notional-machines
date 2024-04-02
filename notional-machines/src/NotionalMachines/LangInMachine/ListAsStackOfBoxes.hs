{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.LangInMachine.ListAsStackOfBoxes where

import Data.Bifunctor (bimap)

import NotionalMachines.Lang.List.Main (List (Empty, Cons), uncons)
import NotionalMachines.Machine.ListAsStackOfBoxes.Main
    ( Stack(..), Box(..), pickUp )

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))



langToNM :: Show a => List a -> Stack
-- langToNM []     = Pallet
-- langToNM (x:xs) = Stack (Box (show x)) (langToNM xs)
langToNM = foldr (Stack . Box . show) Pallet


nilBisim :: Show a => Bisimulation () (List a) () Stack
nilBisim = MkBisim { fLang = const Empty
                   , fNM = const Pallet
                   , alphaA = id
                   , alphaB = langToNM
                   }

consBisim :: Show a => Bisimulation (a, List a) (List a) (Box, Stack) Stack
consBisim = MkBisim { fLang = uncurry Cons
                    , fNM = uncurry Stack
                    , alphaA = bimap (Box . show) langToNM
                    , alphaB = langToNM
                    }

unconsBisim :: Show a => Bisimulation (List a) (Maybe (a, List a)) Stack (Maybe (Box, Stack))
unconsBisim = MkBisim { fLang = uncons
                      , fNM = pickUp
                      , alphaA = langToNM
                      , alphaB = fmap (bimap (Box . show) langToNM)
                      }
