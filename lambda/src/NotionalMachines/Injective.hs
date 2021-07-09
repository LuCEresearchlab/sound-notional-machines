{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module NotionalMachines.Injective where

class Injective a b where
  toNM   :: a -> b
  fromNM :: b -> Maybe a

