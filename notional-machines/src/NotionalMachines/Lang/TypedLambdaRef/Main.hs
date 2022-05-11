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
  Trace(..),
  MachineStateAlaRacket(..),

  isValue,

  typeof,

  parse,
  unparse,

  evalM',
  trace',

  repl,
  replEval,
  replEvalAlaWadler,
  replEvalAlaRacket,

  langPipeline,
  traceAlaRacket) where

import Control.Monad            ((<=<))
import Control.Monad.State.Lazy (StateT, evalStateT, runStateT)

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Location, NameEnv,
                                                            StateRacket (StateRacket), Store,
                                                            Term (..), Type (..),
                                                            emptyStateAlaRacket,
                                                            emptyStateAlaWadler, emptyStore, evalM',
                                                            evalMAlaRacket, evalMAlaWadler, isValue,
                                                            typecheck, typeof)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parse, unparse)
import NotionalMachines.Meta.Steppable                     (SteppableM, traceM)
import NotionalMachines.Utils                              (LangPipeline (LangPipeline), _eval,
                                                            mkCmd, mkLangReplOpts, mkReplEval,
                                                            taplBookMsg)
import Prettyprinter                                       (Pretty (pretty), align, line, list,
                                                            vsep)

-------------------
-- REPL
--------------------
newtype Trace s = Trace [s]
instance Pretty s => Pretty (Trace s) where
  pretty (Trace ss) = list (map (align . (<>) line . pretty) ss)

newtype MachineState = MachineState (Term, Store Location)
instance Pretty MachineState where
  pretty (MachineState (term, store)) = vsep [pretty term, pretty store]

newtype MachineStateAlaWadler = MachineStateAlaWadler (Term, (NameEnv, Store Location))
instance Pretty MachineStateAlaWadler where
  pretty (MachineStateAlaWadler (term, (nameEnv, store))) = vsep [pretty term, pretty nameEnv, pretty store]

newtype MachineStateAlaRacket = MachineStateAlaRacket (Term, StateRacket)
instance Pretty MachineStateAlaRacket where
  pretty (MachineStateAlaRacket (term, StateRacket nameEnv store)) = vsep [pretty term, pretty nameEnv, pretty store]


trace' :: (SteppableM Term (StateT s (Either Error)), Eq (StateT s (Either Error) Term))
       => s -> ((Term, s) -> b) -> Term -> Either Error (Trace b)
trace' initState format = fmap (Trace . fmap format) . runTrace . traceM . fst <=< typecheck
  where runTrace = mapM (`runStateT` initState)


-- These instance are required for tracing because StateTs have to be compared for eq.
instance Eq (StateT (Store Location) (Either Error) Term) where
  (==) = stateEq emptyStore
instance Eq (StateT (NameEnv, Store Location) (Either Error) Term) where
  (==) = stateEq emptyStateAlaWadler
instance Eq (StateT StateRacket (Either Error) Term) where
  (==) = stateEq emptyStateAlaRacket

stateEq :: (Eq (m a), Monad m) => s -> StateT s m a -> StateT s m a -> Bool
stateEq empty s1 s2 = evalStateT s1 empty == evalStateT s2 empty


langPipeline :: LangPipeline Term Type Error (Trace MachineState)
langPipeline = LangPipeline parse evalM' (Just typeof) (trace' emptyStore MachineState)

replEval :: String -> Either Error String
replEval = mkReplEval langPipeline

replEvalAlaWadler :: String -> Either Error String
replEvalAlaWadler = mkReplEval langPipeline { _eval = evalMAlaWadler }

replEvalAlaRacket :: String -> Either Error String
replEvalAlaRacket = mkReplEval langPipeline { _eval = evalMAlaRacket }

repl :: IO ()
repl = mkLangReplOpts [ ("traceAlaWadler", mkCmd . traceAlaWadler)
                      , ("traceAlaRacket", mkCmd . traceAlaRacket) ]
                      "LambdaRef>" (taplBookMsg "13") langPipeline

traceAlaWadler :: String -> Either Error (Trace MachineStateAlaWadler)
traceAlaWadler = trace' emptyStateAlaWadler MachineStateAlaWadler <=< parse

traceAlaRacket :: String -> Either Error (Trace MachineStateAlaRacket)
traceAlaRacket = trace' emptyStateAlaRacket MachineStateAlaRacket <=< parse
