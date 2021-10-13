{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}

{-|
Description : Simply Typed Lambda Calculus with References, Unit, Booleans, and Natural numbers based on TAPL Ch.3 (WIP)
Stability   : experimental

WIP...

-}

module NotionalMachines.Lang.TypedLambdaRef.Main (
  Term(..),
  Type(..),

  isValue,

  typeof,

  parse,
  unparse,

  evalM',

  replEval,
  repl
  ) where

import Control.Monad.State.Lazy (StateT, evalStateT, runStateT)

import Data.Bifunctor (bimap)


import Control.Monad                                       ((<=<))
import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Store, Term (..), Type (..),
                                                            emptyStore, evalM', isValue, typecheck,
                                                            typeof, Location)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parse, unparse)
import NotionalMachines.Meta.Steppable                     (traceM)
import NotionalMachines.Utils                              (mkLangRepl, mkReplEval, pShow,
                                                            taplBookMsg)

--------------------
-- REPL
--------------------

replEval :: String -> Either Error String
replEval = mkReplEval parse evalM' (Just typeof)

trace' :: Term -> Either Error [(String, String)]
trace' = format . runTrace . traceM . fst <=< typecheck
  where runTrace = mapM (`runStateT` emptyStore)
        format = fmap (fmap (bimap pShow pShow))

-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT (Store Location) (Either Error) Term) where
  s1 == s2 = evalStateT s1 emptyStore == evalStateT s2 emptyStore

repl :: IO ()
repl = mkLangRepl "LambdaRef>"
                  parse
                  evalM'
                  trace'
                  (Just typeof)
                  (taplBookMsg "13")
