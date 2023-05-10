{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.Meta.Injective where

import NotionalMachines.Meta.LangToNM (LangToNM)

class (LangToNM lang nm, Monad m) => Injective lang nm m where
  fromNM :: nm -> m lang

