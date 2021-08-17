{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.Meta.Injective where

class Injective a b where
  toNM   :: a -> b
  fromNM :: b -> Maybe a

