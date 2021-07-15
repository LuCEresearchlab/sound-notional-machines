{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module NotionalMachines.Meta.Bijective where

class Bijective a b | a -> b, b -> a where
  toNM   :: a -> b
  fromNM :: b -> a


