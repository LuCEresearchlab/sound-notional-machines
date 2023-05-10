{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.Meta.LangToNM where

class LangToNM lang nm where
  toNM :: lang -> nm

