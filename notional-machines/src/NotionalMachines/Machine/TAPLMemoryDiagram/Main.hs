{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Main ( TAPLMemoryDiagram(..)
                                                       , DLocation(..)
                                                       , Name
                                                       , DTerm(..)) where


import           Data.Map (Map)
import Prettyprinter (Pretty, pretty, (<+>), hcat)

data TAPLMemoryDiagram l =
  TAPLMemoryDiagram {
    memDiaTerm    :: DTerm l
  , memDiaNameEnv :: Map Name (DTerm l)
  , memDiaStore   :: Map (DLocation l) (DTerm l) }
  deriving (Eq, Show)

newtype DLocation a = DLoc a
  deriving (Eq, Show, Ord)

type Name = String

data DTerm l = Leaf String
             | Branch [DTerm l]
             | TLoc (DLocation l)
             deriving (Eq, Show)

instance Pretty a => Pretty (DTerm a) where
  pretty = \case
    Leaf s -> pretty s
    TLoc (DLoc l) -> pretty "Loc" <+> pretty l
    Branch ts -> hcat (fmap pretty ts)
