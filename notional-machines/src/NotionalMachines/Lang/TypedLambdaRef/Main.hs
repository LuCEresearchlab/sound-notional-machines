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

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Location, NameEnv, Store,
                                                            Term (..), Type (..), emptyNameEnv,
                                                            emptyStore, evalM', isValue, typecheck,
                                                            typeof)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parse, unparse)
import NotionalMachines.Meta.Steppable                     (SteppableM, traceM)
import NotionalMachines.Utils                              (LangPipeline (LangPipeline),
                                                            mkLangReplOpts, mkReplEval, taplBookMsg, mkCmd)
import Prettyprinter                                       (Pretty (pretty), align,
                                                            line, list, vsep)

-------------------
-- REPL
--------------------
newtype Trace s = Trace [s]
instance Pretty s => Pretty (Trace s) where
  pretty (Trace ss) = list (map (align . (<>) line . pretty) ss)

newtype MachineState = MachineState (Term, Store Location)
instance Pretty MachineState where
  pretty (MachineState (term, store)) = vsep [pretty term, pretty store]

newtype MachineStateNameEnv = MachineStateNameEnv (Term, (NameEnv, Store Location))
instance Pretty MachineStateNameEnv where
  pretty (MachineStateNameEnv (term, (nameEnv, store))) = vsep [pretty term, pretty nameEnv, pretty store]


trace' :: (SteppableM Term (StateT s (Either Error)), Eq (StateT s (Either Error) Term))
       => s -> ((Term, s) -> b) -> Term -> Either Error [b]
trace' initState format = fmap (fmap format) . runTrace . traceM . fst <=< typecheck
  where runTrace = mapM (`runStateT` initState)


-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT (Store Location) (Either Error) Term) where
  s1 == s2 = evalStateT s1 emptyStore == evalStateT s2 emptyStore


-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT (NameEnv, Store Location) (Either Error) Term) where
  s1 == s2 = evalStateT s1 (emptyNameEnv, emptyStore) == evalStateT s2 (emptyNameEnv, emptyStore)


langPipeline :: LangPipeline Term Type Error (Trace MachineState)
langPipeline = LangPipeline parse evalM' (Just typeof) (fmap Trace . trace' emptyStore MachineState)

replEval :: String -> Either Error String
replEval = mkReplEval langPipeline

repl :: IO ()
repl = mkLangReplOpts [("traceNameEnv", mkCmd . traceNameEnv)]
                      "LambdaRef>" (taplBookMsg "13") langPipeline
  where traceNameEnv :: String -> Either Error (Trace MachineStateNameEnv)
        traceNameEnv = fmap Trace . trace' (emptyNameEnv, emptyStore) MachineStateNameEnv <=< parse
