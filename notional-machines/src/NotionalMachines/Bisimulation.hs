{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Bisimulation where

import           NotionalMachines.Bijective (Bijective)
import qualified NotionalMachines.Bijective as Bij
import           NotionalMachines.Injective (Injective)
import qualified NotionalMachines.Injective as Inj
import NotionalMachines.Steppable

data Bisimulation a' b' a b = Bisim { fLang  :: a' -> b'
                                    , fNM    :: a  -> b
                                    , alphaA :: a' -> a
                                    , alphaB :: b' -> b }

mkBijBisim :: (Bijective l n, Steppable l) => Bisimulation l l n n
mkBijBisim = Bisim { fLang  = step
                   , fNM    = funNM step
                   , alphaA = Bij.toNM
                   , alphaB = Bij.toNM }

mkInjBisim :: (Injective l n) => (l -> l) -> Bisimulation l l n (Maybe n)
mkInjBisim f = Bisim { fLang  = f
                     , fNM    = funMNM f
                     , alphaA = Inj.toNM
                     , alphaB = return . Inj.toNM }

-- class BisimulationC l n | l -> n, n -> l where
--   stepLang  :: l -> l
--   stepNM    :: n -> n
--   alphaA :: l -> n
--   alphaB :: l -> n

-- instance (Bijective l n, Steppable l) => BisimulationC l n where
--   stepLang  = step
--   stepNM = Bij.toNM . step . Bij.fromNM
--   alphaA = Bij.toNM
--   alphaB = alphaA

funNM :: Bijective l n => (l -> l) -> n -> n
funNM f = Bij.toNM . f . Bij.fromNM

funMNM :: Injective l n => (l -> l) -> n -> Maybe n
funMNM f = fmap (Inj.toNM . f) . Inj.fromNM

-- instance (Injective l n, Steppable l) => BisimulationC l n where
--   stepLang  = step
--   stepNM = fmap (Inj.toNM . step) . Inj.fromNM
--   alpha = return . Inj.toNM

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

