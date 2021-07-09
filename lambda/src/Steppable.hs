{-# OPTIONS_GHC -Wall #-}

module Steppable where

import Data.Function (fix)
import Text.Show.Pretty

class Eq a => Steppable a where
  step :: a -> a
  stepEnd :: a -> a
  stepEnd = fixpoint step
  eval :: a -> a
  eval = stepEnd

class Eq a => SteppableM a where
  stepM :: a -> Maybe a
  stepEndM :: a -> Maybe a
  stepEndM = fixpointM stepM
  evalM :: a -> Maybe a
  evalM = stepEndM

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
