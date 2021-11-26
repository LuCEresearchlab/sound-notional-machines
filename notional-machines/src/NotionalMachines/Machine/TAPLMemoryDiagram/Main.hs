{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Main ( TAPLMemoryDiagram(..)
                                                       , Location(..)
                                                       , tAlloc
                                                       , tDeref
                                                       , emptyDiagram
                                                       , tAssign) where

-- There must be a way to uniquely identify the individual memory cells. That could be with a number, as it's the case for memory addresses, but even with the coordinates where the cell is draw on a piece of paper. As long as we're able...
--
-- There must be a way to uniquely identify the individual memory cells. In memory, the cells are individually identifiable using the memory address, but in the case of a diagram, the very drawing of each cell...
--
-- Part of the secret of the bisimulaltion is that `f` is not the one `f` but is "there exists and `f` that implements the semantics of the language". So it is correct if one can come up with an `f`. The proof is by construction by constructing the right `f`. That's definitely needed (e.g. typing and naming in lambdaref->memdiagram). But then how to proof that a NM is definitely incorrect? Proof by constradiction?

import           Data.Map (Map)
import qualified Data.Map as Map

data TAPLMemoryDiagram l t = TAPLMemoryDiagram { taplNameEnv :: Map Name t
                                               , taplStore   :: Map (Location l) t
                                               }
  deriving (Eq, Show)

newtype Location a = Loc a
  deriving (Enum, Eq, Num, Ord, Show)

type Name = String

emptyDiagram :: TAPLMemoryDiagram l t
emptyDiagram = TAPLMemoryDiagram Map.empty Map.empty

-- | Allocate new memory in the store using a given strategy to allocate the new location
tAlloc :: Ord l => (Map (Location l) t -> Location l) -- ^ Strategy used to get new location
                  -> (t, TAPLMemoryDiagram l t) -> (Location l, TAPLMemoryDiagram l t)
tAlloc genNextLoc (t, TAPLMemoryDiagram env s) = (nl, TAPLMemoryDiagram env (Map.insert nl t s))
  where nl = genNextLoc s

tDeref :: Ord l => (Location l, TAPLMemoryDiagram l t) -> Maybe t
tDeref (loc, TAPLMemoryDiagram _ s) = Map.lookup loc s

tAssign :: Ord l => (Location l, t, TAPLMemoryDiagram l t) -> Maybe (TAPLMemoryDiagram l t)
tAssign (l, t, TAPLMemoryDiagram env s) =
  TAPLMemoryDiagram env (Map.adjust (const t) l s) <$ Map.lookup l s
