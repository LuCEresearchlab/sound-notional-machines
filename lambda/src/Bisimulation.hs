{-# OPTIONS_GHC -Wall #-}

module Bisimulation where

import Bijective
import Injective
import Steppable

data Bisimulation a' b' a b = Bisim { fLang  :: a' -> b'
                                    , fNM    :: a  -> b
                                    , alphaA :: a' -> a
                                    , alphaB :: b' -> b }

mkBijBisim :: (Bijective l n, Steppable l) => Bisimulation l l n n
mkBijBisim = Bisim { fLang  = step
                   , fNM    = funNM step
                   , alphaA = Bijective.toNM
                   , alphaB = Bijective.toNM }

mkInjBisim :: (Injective l n, Steppable l) => Bisimulation l l n (Maybe n)
mkInjBisim = Bisim { fLang  = step
                   , fNM    = funMNM step
                   , alphaA = Injective.toNM
                   , alphaB = return . Injective.toNM }

-- class BisimulationC l n | l -> n, n -> l where
--   stepLang  :: l -> l
--   stepNM    :: n -> n
--   alphaA :: l -> n
--   alphaB :: l -> n

-- instance (Bijective l n, Steppable l) => BisimulationC l n where
--   stepLang  = step
--   stepNM = Bijective.toNM . step . Bijective.fromNM
--   alphaA = Bijective.toNM
--   alphaB = alphaA

funNM :: Bijective l n => (l -> l) -> n -> n
funNM f = Bijective.toNM . f . Bijective.fromNM

funMNM :: Injective l n => (l -> l) -> n -> Maybe n
funMNM f = fmap (Injective.toNM . f) . Injective.fromNM

-- instance (Injective l n, Steppable l) => BisimulationC l n where
--   stepLang  = step
--   stepNM = fmap (Injective.toNM . step) . Injective.fromNM
--   alpha = return . Injective.toNM

-- class (Bijective l n, Steppable l) => SteppableNM l n where
--   stepNM :: n -> n
-- stepNM :: (Bijective l n, Steppable l) => (l -> l) -> n -> n
-- stepNM stepLang = toNM . stepLang . fromNM

-- instance (Bijective l n, Steppable l) => SteppableNM l n where
--   stepNM :: n -> n
--   stepNM = toNM . step . fromNM

-- instance Steppable Exp
-- instance Bijective Exp ExpAsTree
-- =>
-- instance Steppable ExpAsTree where

