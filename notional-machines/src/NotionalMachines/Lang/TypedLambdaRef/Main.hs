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
  replTrace,

  replEval,
  repl
  ) where

import Control.Monad.State.Lazy (StateT, evalStateT, runStateT)

import Data.Bifunctor (bimap)


import Control.Monad                                       ((<=<))
import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Error, Store, Term (..), Type (..),
                                                            emptyStore, evalM', isValue, typecheck,
                                                            typeof)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parse, unparse)
import NotionalMachines.Meta.Steppable                     (traceM)
import NotionalMachines.Utils                              (mkHelpCmd, mkRepl, pShow, mkCmd, mkTraceCmd)

--------------------
-- REPL
--------------------

-- | Eval and format output to display in the REPL.
replEval :: String -> Either Error String
replEval = fmap formatTT . eval
  where
    eval :: String -> Either Error (Term, Type)
    eval s = do term <- parse s
                typ  <- typeof term
                val  <- evalM' term
                return (val, typ)

replType :: String -> Either Error String
replType = fmap formatTT . typecheck <=< parse

formatTT :: (Term, Type) -> String
formatTT (term, typ) = pShow term ++ " : " ++ pShow typ

replTrace :: String -> Either Error [(String, String)]
replTrace = format . runTrace . traceM . fst <=< typecheck <=< parse
  where runTrace = mapM (`runStateT` emptyStore)
        format = fmap (fmap (bimap pShow pShow))

-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT Store (Either Error) Term) where
  s1 == s2 = evalStateT s1 emptyStore == evalStateT s2 emptyStore


repl :: IO ()
repl = mkRepl "LambdaRef> " (mkCmd replEval) opts
  where
    opts :: [(String, String -> IO ())]
    opts =
      [ ("help" , mkHelpCmd "13" (map fst opts)),
        ("trace", mkTraceCmd replTrace),
        ("type" , mkCmd replType)
      ]
