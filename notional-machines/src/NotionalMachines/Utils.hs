{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module NotionalMachines.Utils where

import Data.List (intercalate, uncons)

import           Hedgehog       hiding (eval)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (forM, (<=<))

import Control.Monad.Trans       (liftIO)
import Data.Bifunctor            (second)
import Data.Text.Prettyprint.Doc (Pretty, pretty)
import System.Console.Repline    (CompleterStyle (Word), ExitDecision (Exit), HaskelineT,
                                  ReplOpts (..), evalReplOpts)
import Text.Pretty.Simple        (CheckColorTty (..), defaultOutputOptionsDarkBg,
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

sampleN :: Show a => Int -> Gen a -> IO [a]
sampleN n gen = forM [1..n] (const $ Gen.sample gen)

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO b
genAndSolve gen solver =
            do e <- gen
               shortPrint e
               return $ solver e

-------------------------

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


pShowTT :: (Pretty a1, Pretty a2) => (a1, a2) -> String
pShowTT (term, typ) = pShow term ++ " : " ++ pShow typ

taplBookMsg :: String -> String
taplBookMsg bookCh = "The syntax of the language follows TAPL Ch." ++ bookCh


mkReplEval :: (Monad m, Pretty t, Pretty ty) => (String -> m t)
                                             -> (t -> m t)
                                             -> Maybe (t -> m ty)
                                             -> String
                                             -> m String
mkReplEval parse eval' mTypeof s =
    case mTypeof of
      Nothing -> (fmap pShow . (=<<) eval' . parse) s
      Just typeof -> do term <- parse s
                        typ  <- typeof term
                        val  <- eval' term
                        return $ pShowTT (val, typ)


mkLangRepl :: (Pretty t, Show er, Pretty ty, Show a) => String
                                                     -> (String -> Either er t)
                                                     -> (t -> Either er t)
                                                     -> (t -> Either er [a])
                                                     -> Maybe (t -> Either er ty)
                                                     -> String
                                                     -> IO ()
mkLangRepl replBanner parse eval' trace' mTypeof bookMsg =
        mkRepl (replBanner ++ " ") (mkCmd $ mkReplEval parse eval' mTypeof) opts
  where
    opts :: [(String, String -> IO ())]
    opts =
      [ ("help" , mkHelpCmd bookMsg (map fst opts)),
        ("trace", mkTraceCmd (trace' <=< parse))
      ] ++ case mTypeof of Nothing     -> []
                           Just typeof -> [("type", mkTypeCmd typeof parse)]

    mkHelpCmd :: String -> [String] -> String -> IO ()
    mkHelpCmd header cmds = \_ -> putStrLn msg
      where msg = unlines [header, "REPL commands: " ++ intercalate ", " cmds]

    mkCmd :: Show b => (a -> Either b String) -> a -> IO ()
    mkCmd f = either print putStrLn . f

    mkTraceCmd :: (Show b, Show a) => (c -> Either b a) -> c -> IO ()
    mkTraceCmd replTrace = either print shortPrint . replTrace

    mkTypeCmd :: (Show er, Pretty term, Pretty ty) => (term -> Either er ty)
                                                   -> (a -> Either er term)
                                                   -> a
                                                   -> IO ()
    mkTypeCmd typeof parse' = mkCmd $ fmap pShowTT . typecheck <=< parse'
      where typecheck t = (t, ) <$> typeof t

