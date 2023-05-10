{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts #-}

module NotionalMachines.Meta.Bisimulation where

import Control.Monad ((<=<))

import           NotionalMachines.Meta.Bijective (Bijective)
import qualified NotionalMachines.Meta.Bijective as Bij
import           NotionalMachines.Meta.Injective (Injective)
import qualified NotionalMachines.Meta.Injective as Inj
import           NotionalMachines.Meta.LangToNM  (LangToNM (..))
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
                                      , fNM    :: a -> b
                                      , alphaA :: a' -> a
                                      , alphaB :: b' -> b
                                      }

mkBijBisim :: (Bijective l n, Steppable l) => Bisimulation l l n n
mkBijBisim = MkBisim { fLang  = step
                     , fNM    = mkStepBijNM step
                     , alphaA = toNM
                     , alphaB = toNM }

mkInjBisim :: (Injective l1 n1 m, LangToNM l2 n2) => (l1 -> l2) -> Bisimulation l1 l2 n1 (m n2)
mkInjBisim f = MkBisim { fLang  = f
                       , fNM    = mkStepInjNM f
                       , alphaA = toNM
                       , alphaB = pure . toNM }

mkInjBisimM :: (Injective l1 n1 m, LangToNM l2 n2) => (l1 -> m l2) -> Bisimulation l1 (m l2) n1 (m n2)
mkInjBisimM f = MkBisim { fLang  = f
                        , fNM    = mkStepMInjNM f
                        , alphaA = toNM
                        , alphaB = fmap toNM }

mkStepBijNM :: Bijective l n => (l -> l) -> n -> n
mkStepBijNM f = toNM . f . Bij.fromNM

mkStepInjNM :: (Injective l1 n1 m, LangToNM l2 n2) => (l1 -> l2) -> n1 -> m n2
mkStepInjNM f = fmap (toNM . f) . Inj.fromNM

mkStepMInjNM :: (Injective l1 n1 m, LangToNM l2 n2) => (l1 -> m l2) -> n1 -> m n2
mkStepMInjNM f = fmap toNM . f <=< Inj.fromNM

-- instance Steppable Exp
-- instance Bijective Exp ExpAsTree
-- =>
-- instance Steppable ExpAsTree

-- TODO: there's a step that is typeof
--       and there is a step which is 1 step of typeof (requires aux tree?)
