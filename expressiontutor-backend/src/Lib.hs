{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module Lib where

import GHC.Generics
import Data.Aeson
import NotionalMachines.Machine.ExpressionTutor.Main

deriving instance Generic ExpTutorDiagram
deriving instance Generic Node
deriving instance Generic NodeContentElem
deriving instance Generic Edge
deriving instance Generic Plug

instance ToJSON ExpTutorDiagram
instance ToJSON Node
instance ToJSON NodeContentElem
instance ToJSON Edge
instance ToJSON Plug

instance FromJSON ExpTutorDiagram
instance FromJSON Node
instance FromJSON NodeContentElem
instance FromJSON Edge
instance FromJSON Plug


