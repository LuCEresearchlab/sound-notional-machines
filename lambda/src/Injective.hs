{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Injective where

class Injective a b | a -> b, b -> a where
  toNM   :: a -> b
  fromNM :: b -> Maybe a

