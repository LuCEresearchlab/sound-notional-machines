{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures -Wno-orphans #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.TypedLambdaRefTAPLMemoryDiagram where

import           Control.Monad.Identity                              (runIdentity)
import           Data.Bifunctor                                      (bimap, second)
import qualified Data.Map                                            as Map
import           NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Location, Store,
                                                                      Term (..), alloc, assign,
                                                                      deref, nextLocation, StateRacket (StateRacket))
import           NotionalMachines.Machine.TAPLMemoryDiagram.Main     (TAPLMemoryDiagram (..),
                                                                      tAlloc, tAssign, tDeref)
import qualified NotionalMachines.Machine.TAPLMemoryDiagram.Main     as NM (Location (Loc))
import           NotionalMachines.Meta.Bisimulation                  (Bisimulation (..))
import           NotionalMachines.Utils                              (eitherToMaybe, stateToTuple)
import NotionalMachines.Meta.Bijective (Bijective (toNM, fromNM))
import NotionalMachines.Meta.Steppable (stepM)

-- convert from the language store to NM store.
langStoreToNMStore :: Store Location -> TAPLMemoryDiagram Location Term
langStoreToNMStore = TAPLMemoryDiagram Map.empty . Map.mapKeys NM.Loc

langToNMTerm :: (Term,     Store Location) -> (Term,               TAPLMemoryDiagram Location Term)
langToNMTerm = second langStoreToNMStore

langToNMLoc  :: (Location, Store Location) -> (NM.Location Location, TAPLMemoryDiagram Location Term)
langToNMLoc  = bimap NM.Loc  langStoreToNMStore


-- The challenge in building a bisimulation for the operation of memory
-- allocation is the encoding of the monadic State nature of it.
--
-- Memory in lang is represented using State but in the NM it's a product
-- type. These ways to represent state are incompatible so either we adapt the
-- State interface so that the lang functions operate on explicit tuples or we
-- adapt the NM to work via a State monad.
--
-- The fist case is shown below in the next 3 bisimulations.
--
-- An interesting question when we try to go in the other direction: can we
-- write a bisimulation only using State.  The result would be a bisimulation
-- in which one or more vertices of the permutation squares are State and the
-- commutation proof would happen via monadic composition (<=<) instead of
-- function composition!  Monadic bisimulation.  But... No... Apparently that
-- doesn't work or I don't know how to... :-(
--
--
-- (Term,                             ------->  (NM.Location Location,
--  TAPLMemoryDiagram Location Term)             TAPLMemoryDiagram Location Term)
--
--      ^                                                     ^
--      |                                                     |
--      |                                                     |
--      |                                                     |
--
-- (Term, Store Location)               ------->  (Location, Store Location)
--
allocBisim :: Bisimulation (Term, Store Location)
                           (Location, Store Location)
                           (Term, TAPLMemoryDiagram Location Term)
                           (NM.Location Location, TAPLMemoryDiagram Location Term)
allocBisim = MkBisim { fLang  = runIdentity . stateToTuple alloc
                     , fNM    = tAlloc nextLocation
                     , alphaA = langToNMTerm
                     , alphaB = langToNMLoc}

--
-- (Location, Store Location)                       ------------>  Either Error Term
--
--      ^                                                               ^
--      |                                                               |
--      |                                                               |
--      |                                                               |
--
-- (NM.Location Int, TAPLMemoryDiagram Int Term)  ------------>  Maybe Term
--
derefBisim :: Bisimulation (Location, Store Location)
                           (Either Error Term)
                           (NM.Location Location, TAPLMemoryDiagram Location Term)
                           (Maybe Term)
derefBisim = MkBisim { fLang  = fmap fst . stateToTuple deref
                     , fNM    = tDeref
                     , alphaA = langToNMLoc
                     , alphaB = eitherToMaybe }

--
-- ((Location, Term), Store Location)  ------------>  Either Error (Store Location)
--
--      ^                                                               ^
--      |                                                               |
--      |                                                               |
--      |                                                               |
--
-- (NM.Location Location,              ------------>  Maybe (TAPLMemoryDiagram Location Term)
--  Term,
--  TAPLMemoryDiagram Location Term)
--
assignBisim :: Bisimulation ((Location, Term), Store Location)
                            (Either Error (Store Location))
                            (NM.Location Location, Term, TAPLMemoryDiagram Location Term)
                            (Maybe (TAPLMemoryDiagram Location Term))
assignBisim = MkBisim { fLang  = fmap snd . stateToTuple (uncurry assign)
                      , fNM    = tAssign
                      , alphaA = \((l,t),s) -> (NM.Loc l, t, langStoreToNMStore s)
                      , alphaB = eitherToMaybe . fmap langStoreToNMStore }


-- =====================
------
-- Monadic bisimulation - doesn't work :-( or it's not done right... I don't know
------

-- Here we write equivalent bisimulations but now using the State monad. The
-- commutation proof happens via monadic composition (<=<) instead of function
-- composition!  Monadic bisimulation.
--
--
--    Term               ------->  State (TAPLMemoryDiagram Location Term) (NM.Location Location)
--
--      ^                                                     ^
--      |                                                     |
--      |                                                     |
--      |                                                     |
--
--    Term               ------->  State (Store Location) Location
--
-- mAllocBisim :: Bisimulation Term
--                             (State (Store Location) Location)
--                             Term
--                             (State (TAPLMemoryDiagram Location Term) (NM.Location Location))
-- mAllocBisim = MkBisim { fLang  = alloc
--                       , fNM    = tupleToState (return . tAlloc nextLocation)
--                       , alphaA = id
--                       , alphaB = g}


-- g :: State (Store Location) Location -> State (TAPLMemoryDiagram Location Term) (NM.Location Location)
-- g = error "todo"
-- =====================

langToNM :: (Term, StateRacket) -> (Term, TAPLMemoryDiagram Location Term)
langToNM (term, StateRacket env store) = (term, TAPLMemoryDiagram env (Map.mapKeys NM.Loc store))

nmToLang :: (Term, TAPLMemoryDiagram Location Term) -> (Term, StateRacket)
nmToLang (term, TAPLMemoryDiagram env store) = (term, StateRacket env (Map.mapKeys (\(NM.Loc l) -> l) store))

instance Bijective (Term, StateRacket) (Term, TAPLMemoryDiagram Location Term) where
  toNM   = langToNM
  fromNM = nmToLang


bisim :: Bisimulation (Term, StateRacket)
                      (Either Error (Term, StateRacket))
                      (Term, TAPLMemoryDiagram Location Term)
                      (Either Error (Term, TAPLMemoryDiagram Location Term))
bisim = MkBisim { fLang  = stateToTuple stepM
                , fNM    = fmap toNM . stateToTuple stepM . fromNM
                , alphaA = toNM
                , alphaB = fmap toNM }
