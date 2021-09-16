{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Utils where

import Data.List (intercalate, uncons)

import           Hedgehog       hiding (eval)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (forM_)

import Control.Monad.Trans             (liftIO)
import Data.Bifunctor                  (second)
import Data.Text.Prettyprint.Doc       (Pretty, pretty)
import NotionalMachines.Meta.Steppable (Steppable, eval, trace)
import System.Console.Repline          (CompleterStyle (Word), ExitDecision (Exit), HaskelineT,
                                        ReplOpts (..), evalReplOpts)
import Text.Pretty.Simple              (CheckColorTty (..), defaultOutputOptionsDarkBg,
                                        outputOptionsCompact, pPrintOpt)


maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither l = maybe (Left l) Right

pShow :: Pretty a => a -> String
pShow = show . pretty

shortPrint :: Show a => a -> IO ()
shortPrint = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = False}

------- Generators utils ----------

genName :: MonadGen m => m String
genName = Gen.list (Range.singleton 1) $ Gen.element ['a'..'z']

sample :: Gen a -> IO a
sample = Gen.sample

printSample :: Show a => Int -> Gen a -> IO ()
printSample n gen = forM_ [1..n] (\_ -> shortPrint =<< Gen.sample gen)

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO b
genAndSolve gen solver =
            do e <- gen
               shortPrint e
               return $ solver e

-- REPL

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

mkHelpCmd :: String -> [String] -> String -> IO ()
mkHelpCmd bookCh opts = \_ -> putStrLn msg
  where
    msg = unlines ["The syntax of the language follows TAPL Ch." ++ bookCh,
                   "REPL commands: " ++ intercalate ", " opts]

mkCmd :: Show b => (a -> Either b String) -> a -> IO ()
mkCmd f = either print putStrLn . f

mkTraceCmd :: (Show b, Show a) => (c -> Either b a) -> c -> IO ()
mkTraceCmd replTrace = either print shortPrint . replTrace

handleEr :: (a -> IO ()) -> Either String a -> IO ()
handleEr = either (\l -> putStrLn $ "Error: " ++ l)

