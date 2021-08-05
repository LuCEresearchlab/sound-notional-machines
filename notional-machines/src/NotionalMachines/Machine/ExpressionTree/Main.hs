{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Machine.ExpressionTree.Main where

data ExpAsTree = Box String
               | BinaryBox ExpAsTree ExpAsTree
               | LambdaBox String ExpAsTree
               deriving (Eq, Show)

