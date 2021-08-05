{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Meta.Bisimulation where

import           NotionalMachines.Meta.Bijective (Bijective)
import qualified NotionalMachines.Meta.Bijective as Bij
import           NotionalMachines.Meta.Injective (Injective)
import qualified NotionalMachines.Meta.Injective as Inj
import           NotionalMachines.Meta.Steppable (Steppable, step)

--------------------------
------ Bisimulation ------
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

data Bisimulation a' b' a b = MkBisim { fLang  :: a' -> b'
                                      , fNM    :: a  -> b
                                      , alphaA :: a' -> a
                                      , alphaB :: b' -> b }

mkBijBisim :: (Bijective l n, Steppable l) => Bisimulation l l n n
mkBijBisim = MkBisim { fLang  = step
                   , fNM    = stepNM step
                   , alphaA = Bij.toNM
                   , alphaB = Bij.toNM }

mkInjBisim :: (Injective l n) => (l -> l) -> Bisimulation l l n (Maybe n)
mkInjBisim f = MkBisim { fLang  = f
                     , fNM    = stepMNM f
                     , alphaA = Inj.toNM
                     , alphaB = return . Inj.toNM }

stepNM :: Bijective l n => (l -> l) -> n -> n
stepNM f = Bij.toNM . f . Bij.fromNM

stepMNM :: Injective l n => (l -> l) -> n -> Maybe n
stepMNM f = fmap (Inj.toNM . f) . Inj.fromNM

-- instance Steppable Exp
-- instance Bijective Exp ExpAsTree
-- =>
-- instance Steppable ExpAsTree

-- TODO: there's a step that is typeof
--       and there is a step which is 1 step of typeof (requires aux tree?)
