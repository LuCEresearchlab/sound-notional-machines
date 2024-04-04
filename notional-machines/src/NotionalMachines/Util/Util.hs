{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module NotionalMachines.Util.Util where

import Control.Monad.State.Lazy (State, StateT (StateT), runState, runStateT, state)

import Data.List       (intercalate, uncons)
import Data.List.Split (splitOn)

import           Data.Map (Map)
import qualified Data.Map as Map

import Data.Colour.RGBSpace (RGB, uncurryRGB)
import Data.Colour.SRGB     (sRGB, sRGB24show)

import Prettyprinter (Pretty, pretty)

import Text.Pretty.Simple (CheckColorTty (..), defaultOutputOptionsDarkBg, outputOptionsCompact,
                           pPrintOpt)


maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

maybeAt :: [a] -> Integer -> Maybe a
maybeAt xs i = lookup i (zip [0..] xs)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither l = maybe (Left l) Right

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace xs ys = intercalate ys . splitOn xs

mapFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapFirst _ _ []             = []
mapFirst p f (x : xs) | p x = f x : xs
mapFirst p f (x : xs)       =   x : mapFirst p f xs

mapFirstM :: Monad m => (a -> Bool) -> (a -> m a) -> [a] -> m [a]
mapFirstM _ _ []             = return []
mapFirstM p f (x : xs) | p x = (  : xs) <$> f x
mapFirstM p f (x : xs)       = (x :   ) <$> mapFirstM p f xs

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

nextKey :: (Enum k, Ord k, Num k) => Map k v -> k
nextKey = succ . foldl max (-1) . Map.keys
