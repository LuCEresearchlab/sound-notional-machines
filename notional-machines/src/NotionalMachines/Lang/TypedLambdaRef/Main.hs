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

import Control.Monad            ((<=<))
import Control.Monad.State.Lazy (StateT, evalStateT, runStateT)

import Data.Bifunctor (bimap)


import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Store, Term (..), Type (..), emptyStore,
                                                            evalM', isValue, typecheck, typeof)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parse, unparse)
import NotionalMachines.Meta.Steppable                     (traceM)
import NotionalMachines.Utils                              (handleEr, mkHelpMsg, mkRepl, pShow,
                                                            shortPrint)


eval :: String -> Either String (Term, Type)
eval s = do term <- parse s
            typ  <- typeof term
            val  <- evalM' term
            return (val, typ)

-- | Eval and format output to display in the REPL.
replEval :: String -> Either String String
replEval = fmap formatTT . eval

replType :: String -> Either String String
replType = fmap formatTT . typecheck <=< parse

formatTT :: (Term, Type) -> String
formatTT (term, typ) = pShow term ++ " : " ++ pShow typ

replTrace :: String -> Either String [(String, String)]
replTrace = format . runTrace . traceM . fst <=< typecheck <=< parse
  where runTrace = mapM (`runStateT` emptyStore)
        format = fmap (fmap (bimap pShow pShow))

-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT Store (Either String) Term) where
  s1 == s2 = evalStateT s1 emptyStore == evalStateT s2 emptyStore


--------------------
-- REPL
--------------------
repl :: IO ()
repl = mkRepl "LambdaRef> " evalCmd opts
  where
    opts :: [(String, String -> IO ())]
    opts =
      [ ("help" , helpCmd),
        ("type" , typeCmd),
        ("trace", traceCmd)
      ]

    evalCmd :: String -> IO ()
    evalCmd = handleEr putStrLn . replEval

    traceCmd :: String -> IO ()
    traceCmd = handleEr shortPrint . replTrace

    typeCmd :: String -> IO ()
    typeCmd = handleEr putStrLn . replType

    helpCmd :: String -> IO ()
    helpCmd _ = putStrLn $ mkHelpMsg "13" (map fst opts)

