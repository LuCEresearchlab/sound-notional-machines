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
  trace,

  replEval,
  repl
  ) where

import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Lazy (StateT, evalStateT, runStateT)

import Data.Bifunctor (bimap)
import Data.List (intercalate)

import System.Console.Repline (HaskelineT, ReplOpts(..), CompleterStyle(..), WordCompleter, ExitDecision(..), evalReplOpts)

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Term(..), Type(..), Store, emptyStore, typeof, typecheck, evalM', isValue)
import NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (parse, unparse)
import NotionalMachines.Meta.Steppable (traceM)
import NotionalMachines.Utils (shortPrint, pShow)


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

trace :: String -> Either String [(String, String)]
trace = format . runTrace . traceM . fst <=< typecheck <=< parse
  where runTrace = mapM (`runStateT` emptyStore)
        format = fmap (fmap (bimap pShow pShow))

-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT Store (Either String) Term) where
  s1 == s2 = evalStateT s1 emptyStore == evalStateT s2 emptyStore


------------
type Repl a = HaskelineT IO a

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer _ = return []
-- completer n = do let names = ["kirk", "spock", "mccoy"]
--                  return $ filter (isPrefixOf n) names

handleEr :: (a -> IO ()) -> Either String a -> IO ()
handleEr = either (\l -> putStrLn $ "Error: " ++ l)

-- Commands

-- Evaluation : handle each line user inputs
evalCmd :: String -> Repl ()
evalCmd = liftIO . handleEr putStrLn . replEval

helpCmd :: String -> Repl ()
helpCmd _ = liftIO $ putStrLn msg
  where msg = unlines ["The syntax of the language follows TAPL Ch.13",
                       "REPL commands: " ++ intercalate ", " (map fst opts)]

typeCmd :: String -> Repl ()
typeCmd = liftIO . handleEr putStrLn . replType

traceCmd :: String -> Repl ()
traceCmd = liftIO . handleEr shortPrint . trace

opts :: [(String, String -> Repl ())]
opts =
  [ ("help" , helpCmd),
    ("type" , typeCmd),
    ("trace", traceCmd)
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

final :: Repl ExitDecision
final = do liftIO $ putStrLn "Goodbye!"
           return Exit

repl :: IO ()
repl = evalReplOpts $ ReplOpts
  { banner           = const (pure "LambdaRef> ")
  , command          = evalCmd
  , options          = opts
  , prefix           = Just ':'
  , multilineCommand = Nothing
  , tabComplete      = Word completer
  , initialiser      = ini
  , finaliser        = final
  }


