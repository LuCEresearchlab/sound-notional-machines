{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-pattern-synonym-signatures -Wno-unused-do-bind #-}

module Utils where

import Data.List (uncons)
import Data.Function (fix)

import Text.Show.Pretty

maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons

-- successively apply g until the result doesn't change
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint g = fix (\rec x -> if g x == x then x else rec (g x))

-- successively apply g until the result doesn't change
fixpointM :: (Eq (m a), Monad m) => (a -> m a) -> a -> m a
fixpointM g = fix (\rec x -> if g x == return x then return x else rec =<< g x)

showFixpoint :: (Show a, Eq a) => (a -> a) -> a -> IO ()
showFixpoint g x =
  do putStrLn "--- Step ---"
     pPrint x
     if g x == x then return ()
     else showFixpoint g (g x)
