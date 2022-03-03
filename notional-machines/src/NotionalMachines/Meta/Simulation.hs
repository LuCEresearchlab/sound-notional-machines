{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Meta.Simulation where

import           NotionalMachines.Meta.Bijective (Bijective)
import qualified NotionalMachines.Meta.Bijective as Bij
import           NotionalMachines.Meta.Injective (Injective)
import qualified NotionalMachines.Meta.Injective as Inj
import           NotionalMachines.Meta.Steppable (Steppable, step)

--------------------------
------ Simulation ------
--------------------------
--
--    a ----fNM---> b
--
--    ^             ^
--    |             |
--  alphaA        alphaB
--    |             |
--    |             |
--
--    a' --fLang--> b'
--
--    a : Abstraction        (E.g., List)            == Notional machine
--    a': Concrete construct (E.g., ArrayList)       == Programming language
--    f : Abstract program state transition function == Notional machine "process"
--    f': Concrete program state transition function (e.g. reduction)
-- alpha: Abstraction function
--
-- The abstraction is correct if:
-- alphaB . f' == f . alphaA
--------------------------

data Simulation a' b' a b = MkSim { fLang  :: a' -> b'
                                      , fNM    :: a -> b
                                      , alphaA :: a' -> a
                                      , alphaB :: b' -> b
                                      }

mkBijSim :: (Bijective l n, Steppable l) => Simulation l l n n
mkBijSim = MkSim { fLang  = step
                     , fNM    = stepNM step
                     , alphaA = Bij.toNM
                     , alphaB = Bij.toNM }

mkInjSim :: (Injective l n) => (l -> l) -> Simulation l l n (Maybe n)
mkInjSim f = MkSim { fLang  = f
                       , fNM    = stepMNM f
                       , alphaA = Inj.toNM
                       , alphaB = pure . Inj.toNM }

stepNM :: Bijective l n => (l -> l) -> n -> n
stepNM f = Bij.toNM . f . Bij.fromNM

stepMNM :: Injective l n => (l -> l) -> n -> Maybe n
stepMNM f = fmap (Inj.toNM . f) . Inj.fromNM
