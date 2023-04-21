{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Main ( TAPLMemoryDiagram(..)
                                                       , DLocation(..)
                                                       , Name
                                                       , DTerm(..)) where

-- There must be a way to uniquely identify the individual memory cells. That could be with a number, as it's the case for memory addresses, but even with the coordinates where the cell is draw on a piece of paper. As long as we're able...
--
-- There must be a way to uniquely identify the individual memory cells. In memory, the cells are individually identifiable using the memory address, but in the case of a diagram, the very drawing of each cell...
--
-- Part of the secret of the bisimulaltion is that `f` is not the one `f` but is "there exists and `f` that implements the semantics of the language". So it is correct if one can come up with an `f`. The proof is by construction by constructing the right `f`. That's definitely needed (e.g. typing and naming in lambdaref->memdiagram). But then how to proof that a NM is definitely incorrect? Proof by constradiction?

import Data.Map      (Map)
import Prettyprinter (Pretty, hcat, pretty, (<+>))

data TAPLMemoryDiagram l = TAPLMemoryDiagram { memDiaTerm    :: DTerm l
                                             , memDiaNameEnv :: Map Name (DTerm l)
                                             , memDiaStore   :: Map (DLocation l) (DTerm l)
                                             }
  deriving (Eq, Show)

newtype DLocation a = DLoc a
  deriving (Eq, Ord, Show)

type Name = String

data DTerm l = Leaf String
             | Branch [DTerm l]
             | TLoc (DLocation l)
  deriving (Eq, Show)

instance Pretty a => Pretty (DTerm a) where
  pretty = \case
    Leaf s        -> pretty s
    TLoc (DLoc l) -> pretty "Loc" <+> pretty l
    Branch ts     -> hcat (fmap pretty ts)
