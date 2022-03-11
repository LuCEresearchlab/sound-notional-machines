{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module NotionalMachines.Utils where

import Data.Colour.RGBSpace (uncurryRGB)

import Hedgehog ( Gen, MonadGen )
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Control.Monad            (forM, (<=<))
import Control.Monad.State.Lazy (State, StateT (StateT), runState, runStateT, state)
import Control.Monad.Trans      (liftIO)

import Data.Bifunctor (second)
import Data.List      (intercalate, uncons, intersperse)
import Data.List.Split (splitOn, chunksOf)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Data (Typeable)

import System.Console.Repline (CompleterStyle (Word), ExitDecision (Exit), HaskelineT,
                               ReplOpts (..), evalReplOpts)

import Prettyprinter (Doc, Pretty, colon, dot, pretty, squotes, (<+>))

import qualified Text.Parsec        as Parsec (ParseError)
import           Text.Pretty.Simple (CheckColorTty (..), defaultOutputOptionsDarkBg,
                                     outputOptionsCompact, pPrintOpt)

import Diagrams.Prelude hiding (uncons, dot)
import Diagrams.TwoD.Text (Text)
import Diagrams.Backend.SVG (renderSVG, SVG)
import qualified Diagrams.Backend.Rasterific as Rasterific ( B )
import Diagrams.Backend.Rasterific.CmdLine ( mainWith )
import Diagrams.Backend.CmdLine (DiagramOpts(..), DiagramLoopOpts (..), mainRender)

---- Error types ----

data Error = ParseError Parsec.ParseError
           | TypeError String
           | RuntimeError String
  deriving (Eq, Show)

instance Pretty Error where
    pretty (ParseError parsecError) =    "Parse error" <+> pretty (show parsecError)
    pretty (TypeError m)            =    "Type error:" <+> pretty m
    pretty (RuntimeError m)         = "Runtime error:" <+> pretty m

instance Pretty Parsec.ParseError where
    pretty = pretty . show

typeOfEq :: (Pretty term, Pretty typ1, Pretty typ2, Eq typ1) =>
            (term -> Either Error typ1) -> term -> term -> typ1 -> typ2 -> Either Error typ2
typeOfEq rec ctx t typ typ3 = do typ1 <- rec t
                                 if typ1 == typ then return typ3
                                                else mismatch ctx typ typ1 t

mismatch :: (Pretty term, Pretty typ1, Pretty typ2) =>
            term -> typ1 -> typ2 -> term -> Either Error ty
mismatch ctxTerm expected found term = Left . TypeError . show $
       "expected" <+> q expected <+>
            "but" <+> q term     <+>
       "has type" <+> q found    <+>
  "in expression" <+> q ctxTerm   <> dot
      where q :: Pretty p => p -> Doc c
            q = squotes . pretty
----

maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither l = maybe (Left l) Right

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace xs ys = intercalate ys . splitOn xs

-- | Equivalent to Map.map but with a monadic mapM behavior.
mapMapM :: (Ord k, Monad m) => (a -> m b) -> Map k a -> m (Map k b)
mapMapM f = fmap Map.fromList . mapM (\(a, b) -> (a, ) <$> f b) . Map.toList

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

------- Diagrams utils ----------

framed :: (Enveloped d, Transformable d, TrailLike d, Monoid d, V d ~ V2) => d -> d
framed d = d <> boundingRect d

framedRound :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) => QDiagram b V2 Double Any -> QDiagram b V2 Double Any
framedRound d = d # centerXY <> roundedRect w h r
  where
    w = width d
    h = height d
    r = 0.5

diaSeq :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) =>
          Int -> Double -> Double -> [QDiagram b V2 Double Any] -> QDiagram b V2 Double Any
diaSeq n w h =      hcat . map alignT . (\ds -> intersperse (vrule (height ds)) ds)
             . map (vcat .              (\ds -> intersperse (hrule (width  ds)) ds))
             . chunksOf n
             . zipWith (addIndex 0.9) [0..]
             . map withSpacing
  where withSpacing = withEnvelope (rect w h :: D V2 Double) . centerXY
        rectPerc p d = rect (p * width d) (p * height d) # lw 0
        addIndex perc i d = d <> (rectPerc perc d # alignBR <> idx i) # centerXY
          where idx j = rectPerc (1-perc) d <> text (show j) # fontSizeL ((1-perc) * height d)

renderDiagram :: (Show n, Typeable n, RealFloat n) =>
                 FilePath -> n -> QDiagram SVG V2 n Any -> IO ()
renderDiagram fileName w = renderSVG fileName (mkWidth w)

-- Rendering with Rasterific
renderD :: String -> Diagram Rasterific.B -> IO ()
renderD fileName = mainRender (dft fileName)
  where dft fileName = (DiagramOpts (Just 640) (Just 480) fileName, DiagramLoopOpts False Nothing 0)

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


data LangPipeline term typ err trace =
  LangPipeline { _parse   :: String -> Either err term
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
