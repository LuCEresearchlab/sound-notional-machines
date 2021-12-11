{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module NotionalMachines.Utils where

import Data.Colour.SRGB (sRGB24show, sRGB, RGB)
import Data.Colour.RGBSpace (uncurryRGB)

import           Hedgehog       hiding (eval)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Control.Monad            (forM, (<=<))
import Control.Monad.State.Lazy (State, StateT (StateT), runState, runStateT, state)
import Control.Monad.Trans      (liftIO)

import Data.Bifunctor (second)
import Data.List      (intercalate, uncons)

import System.Console.Repline (CompleterStyle (Word), ExitDecision (Exit), HaskelineT,
                               ReplOpts (..), evalReplOpts)

import Data.Text.Prettyprint.Doc (Pretty, pretty, (<+>))

import Text.Parsec        (ParseError)
import Text.Pretty.Simple (CheckColorTty (..), defaultOutputOptionsDarkBg, outputOptionsCompact,
                           pPrintOpt)

---- Error types ----

data Error = ParseError ParseError
               | TypeError String
               | RuntimeError String
  deriving (Eq, Show)

instance Pretty Error where
    pretty (ParseError parsecError) = "Parse error" <+> pretty (show parsecError)
    pretty (TypeError m)            = "Type error:" <+> pretty m
    pretty (RuntimeError m)         = "Runtime error:" <+> pretty m

instance Pretty ParseError where
    pretty = pretty . show

typeOfEq :: (Pretty term, Pretty typ1, Pretty typ2, Eq typ1) =>
            (term -> Either Error typ1) -> term -> term -> typ1 -> typ2 -> Either Error typ2
typeOfEq rec ctx t typ typ3 = do typ1 <- rec t
                                 if typ1 == typ then return typ3
                                                else mismatch ctx typ typ1 t

mismatch :: (Pretty term, Pretty typ1, Pretty typ2) => term -> typ1 -> typ2 -> term -> Either Error ty
mismatch ctxTerm expected found term = Left . TypeError . show $
     "expected '" <> pretty expected
  <> "' but '" <> pretty term
  <> "' has type '" <> pretty found
  <> "' in expression '" <> pretty ctxTerm <> "'."
----

maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither l = maybe (Left l) Right

-- Turns a function that operatees on State into a function that operates on tuples.
stateToTuple :: (a -> StateT s m b) -> (a, s) -> m (b, s)
stateToTuple f (a, s) = runStateT (f a) s

-- Turns a function that operatees on tuples into a function that operates on State.
tupleToState :: ((a, s) -> m (b, s)) -> a -> StateT s m b
tupleToState f a = StateT (curry f a)

stateToStateT :: Monad m => State s a -> StateT s m a
stateToStateT = state . runState

prettyToString :: Pretty a => a -> String
prettyToString = show . pretty

shortPrint :: Show a => a -> IO ()
shortPrint = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = False}

showRGB :: RGB Double -> String
showRGB = sRGB24show . uncurryRGB sRGB

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



taplBookMsg :: String -> String
taplBookMsg bookCh = "The syntax of the language follows TAPL Ch." ++ bookCh


data LangPipeline term typ err trace = LangPipeline { _parse   :: String -> Either err term
                                                    , _eval    :: term -> Either err term
                                                    , _mTypeof :: Maybe (term -> Either err typ)
                                                    , _trace   :: term -> Either err trace
                                                    }

data TypedTerm t ty = TypedTerm t ty
instance (Pretty t, Pretty ty) => Pretty (TypedTerm t ty) where
    pretty (TypedTerm t ty) = pretty t <+> ":" <+> pretty ty


mkReplEval :: (Pretty term, Pretty ty) => LangPipeline term ty er trace
                                    -> String
                                    -> Either er String
mkReplEval (LangPipeline parse' eval' mTypeof _)        = case mTypeof of
  Nothing      -> fmap format . (=<<) eval' . parse'
  Just typeof' -> \s -> do term <- parse' s
                           typ  <- typeof' term
                           val  <- eval' term
                           (return . format . TypedTerm val) typ
  where format :: Pretty a => a -> String
        format = prettyToString



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
mkLangReplOpts otherCmds replBanner bookMsg pipe @ (LangPipeline parse _ mTypeof trace) =
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

    mkTypeCmd :: (Pretty er, Pretty term, Pretty ty) => (term -> Either er ty)
                                                     -> (a -> Either er term)
                                                     -> a
                                                     -> IO ()
    mkTypeCmd typeof parse' = mkCmd . (\s -> do term <- parse' s
                                                typ  <- typeof term
                                                return $ TypedTerm term typ)

mkCmd :: (Pretty e, Pretty b) => Either e b -> IO ()
mkCmd = either (print . pretty) (print . pretty)
