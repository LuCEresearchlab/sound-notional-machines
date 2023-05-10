{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FunctionalDependencies #-}

module NotionalMachines.Meta.Bijective where

import NotionalMachines.Meta.LangToNM (LangToNM)

class LangToNM lang nm => Bijective lang nm | lang -> nm, nm -> lang where
  fromNM :: nm -> lang


