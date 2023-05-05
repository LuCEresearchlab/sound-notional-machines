{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveTraversable #-}

module NotionalMachines.Util.REPL where

import Control.Monad       ((<=<))
import Control.Monad.Trans (liftIO)

import Data.Bifunctor (second)
import Data.List      (intercalate)

import System.Console.Repline (CompleterStyle (Word), ExitDecision (Exit), HaskelineT,
                               ReplOpts (..), evalReplOpts)

import Prettyprinter (Pretty, colon, pretty, (<+>))

import NotionalMachines.Util.Util (prettyToString)


type Repl a = HaskelineT IO a
mkRepl :: String
       -> (String -> IO ())           -- ^ eval function
       -> [(String, String -> IO ())] -- ^ a function per repl option
       -> IO ()
mkRepl replBanner evalCmd opts = evalReplOpts $ ReplOpts
  { banner           = const (pure replBanner)
  , command          = liftIO . evalCmd
  , options          = map (second (liftIO .)) opts
  , prefix           = Just ':'
  , multilineCommand = Nothing
  , tabComplete      = (Word . const . return) []
  , initialiser      = liftIO $ putStrLn "Welcome!"
  , finaliser        = do liftIO $ putStrLn "Goodbye!"
                          return Exit
  }



taplBookMsg :: String -> String
taplBookMsg bookCh = "The syntax of the language follows TAPL Ch." ++ bookCh


data LangPipeline term typ err trace = LangPipeline { _parse   :: String -> Either err term
                                                    , _eval    :: term -> Either err term
                                                    , _mTypeof :: Maybe (term -> Either err typ)
                                                    , _trace   :: term -> Either err trace
                                                    }

data TypedTerm ty t = TypedTerm ty t
  deriving (Foldable, Functor, Traversable)
instance (Pretty t, Pretty ty) => Pretty (TypedTerm ty t) where
    pretty (TypedTerm ty t) = pretty t <+> colon <+> pretty ty

typedTerm :: Monad f => (a -> f ty) -> a -> f (TypedTerm ty a)
typedTerm typeof' term = TypedTerm <$> typeof' term <*> return term

mkReplEval :: (Pretty term, Pretty ty) =>
              LangPipeline term ty er trace -> String -> Either er String
mkReplEval (LangPipeline parse' eval' mTypeOf _) = case mTypeOf of
  Just typeof' -> format <=< mapM eval' <=< typedTerm typeof' <=< parse'
  Nothing      -> format <=<      eval'                       <=< parse'

format :: Pretty a => a -> Either er String
format = return . prettyToString

mkLangRepl :: (Pretty term, Pretty err, Pretty ty, Pretty trace) =>
              String -- ^ REPL banner
           -> String -- ^ Help command message
           -> LangPipeline term ty err trace -- ^ lang pipeline functions
           -> IO ()
mkLangRepl = mkLangReplOpts []

mkLangReplOpts :: (Pretty term, Pretty err, Pretty ty, Pretty trace) =>
                  [(String, String -> IO ())] -- ^ Extra commands
               -> String -- ^ REPL banner
               -> String -- ^ Help command message
               -> LangPipeline term ty err trace -- ^ lang pipeline functions
               -> IO ()
mkLangReplOpts otherCmds replBanner bookMsg pipe@(LangPipeline parse _ mTypeof trace) =
        mkRepl (replBanner ++ " ") (mkCmd . mkReplEval pipe) opts
  where
    opts :: [(String, String -> IO ())]
    opts =
      -- :help
      [("help" , mkHelpCmd bookMsg (map fst opts))]
      -- :type
      ++ maybe [] (\typeof -> [("type", mkTypeCmd typeof parse)]) mTypeof
      -- :trace
      ++ [("trace" , mkCmd . (trace <=< parse))]
      -- others
      ++ otherCmds

    mkHelpCmd :: String -> [String] -> String -> IO ()
    mkHelpCmd header cmds _ = putStrLn msg
      where msg = unlines [header, "REPL commands: " ++ intercalate ", " cmds]

    mkTypeCmd :: (Pretty er, Pretty term, Pretty ty) =>
                 (term -> Either er ty) -> (a -> Either er term) -> a -> IO ()
    mkTypeCmd typeof parse' = mkCmd . (typedTerm typeof <=< parse')

mkCmd :: (Pretty e, Pretty b) => Either e b -> IO ()
mkCmd = either (print . pretty) (print . pretty)

