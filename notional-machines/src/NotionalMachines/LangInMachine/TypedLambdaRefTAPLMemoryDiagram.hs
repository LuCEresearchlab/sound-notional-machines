{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures -Wno-orphans #-}
{-# LANGUAGE TupleSections #-}

module NotionalMachines.LangInMachine.TypedLambdaRefTAPLMemoryDiagram where

import           Control.Monad.Identity                              (runIdentity, Identity (Identity))
import           Data.Bifunctor                                      (bimap, second)
import qualified Data.Map                                            as Map
import           NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Location, Store,
                                                                      Term (..), alloc, assign,
                                                                      deref, nextLocation)
import           NotionalMachines.Machine.TAPLMemoryDiagram.Main     (TAPLMemoryDiagram (..),
                                                                      tAlloc, tAssign, tDeref)
import qualified NotionalMachines.Machine.TAPLMemoryDiagram.Main     as NM (Location (Loc))
import           NotionalMachines.Meta.Bisimulation                  (Bisimulation (..))
import           NotionalMachines.Utils                              (eitherToMaybe, stateToTuple, tupleToState)
import Control.Monad.State.Lazy (State)

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
-- Memory in lang is represented using State but in the NM it's an product
-- type. These ways to represent state are incompatible so either we adapt the
-- State interface so that the lang functions operate on explicit tuples or we
-- adapt the NM to work via a State monad.
--
-- The fist case is shown below in the next 3 bisimulations. The later will
-- result in a bisimulation in which the vertices of the permutation squares
-- are State and the commutation proof happens via monadic composition (<=<)
-- instead of function composition!  Monadic bisimulation.
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
mAllocBisim :: Bisimulation Term
                            (State (Store Location) Location)
                            Term
                            (State (TAPLMemoryDiagram Location Term) (NM.Location Location))
mAllocBisim = MkBisim { fLang  = alloc
                      , fNM    = tupleToState (return . tAlloc nextLocation)
                      , alphaA = id
                      , alphaB = langToNMLoc}
