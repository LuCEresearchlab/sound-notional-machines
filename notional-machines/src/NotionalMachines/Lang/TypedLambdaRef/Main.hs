{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts  #-}
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

import Control.Monad            ((<=<))
import Control.Monad.State.Lazy (StateT, evalStateT, runStateT)

import Data.Bifunctor (bimap)

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Location, NameEnv, Store,
                                                            Term (..), Type (..), emptyNameEnv,
                                                            emptyStore, evalM', isValue, typecheck,
                                                            typeof)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parse, unparse)
import NotionalMachines.Meta.Steppable                     (SteppableM, traceM)
import NotionalMachines.Utils                              (mkLangRepl, mkReplEval, pShow,
                                                            taplBookMsg)

--------------------
-- REPL
--------------------

replEval :: String -> Either Error String
replEval = mkReplEval parse evalM' (Just typeof)


trace' :: (SteppableM Term (StateT s (Either Error)), Eq (StateT s (Either Error) Term))
       => s -> ((Term, s) -> b) -> Term -> Either Error [b]
trace' initState format = fmap (fmap format) . runTrace . traceM . fst <=< typecheck
  where runTrace = mapM (`runStateT` initState)

formatTermStore :: (Term, Store Location) -> (String, String)
formatTermStore = bimap pShow pShow

formatTermEnvStore :: (Term, (NameEnv, Store Location)) -> (String, String)
formatTermEnvStore (term, (env, store)) = (pShow term, pShow env ++ "\n" ++ pShow store)


-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT (Store Location) (Either Error) Term) where
  s1 == s2 = evalStateT s1 emptyStore == evalStateT s2 emptyStore


-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT (NameEnv, Store Location) (Either Error) Term) where
  s1 == s2 = evalStateT s1 (emptyNameEnv, emptyStore) == evalStateT s2 (emptyNameEnv, emptyStore)



repl :: IO ()
repl = mkLangRepl "LambdaRef>"
                  parse
                  evalM'
                  (Just typeof)
                  [("trace", trace' emptyStore formatTermStore),
                   ("traceNameEnv", trace' (emptyNameEnv, emptyStore) formatTermEnvStore)]
                  (taplBookMsg "13")
