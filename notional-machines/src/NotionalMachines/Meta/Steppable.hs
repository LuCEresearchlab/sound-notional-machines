{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.Meta.Steppable where

import Data.Function (fix)
import Control.Monad (join)

{- | A type @a@ is Steppable if it provides a function @step@ which can be
 - repeatedly applied to a value of type @a@ until you reach a fix point (until
 - the value doesn't change).
-}
class Eq a => Steppable a where
  -- | A function that represents a stepwise change of a value towards a
  -- fixpoint.
  step :: a -> a

  -- | Successively apply `step` to a given argument until the result doesn't
  -- change.
  eval :: a -> a
  eval = fixpoint step

  -- | Returns a list of repeated applications of `step` to an argument
  -- stopping when the fixpoint is reached.
  trace :: a -> [a]
  trace = allPoints step


{- | Similar to `Steppable` but for step functions that produce a monadic value. -}
class (Monad m, Eq (m a)) => SteppableM a m where
  stepM :: a -> m a

  evalM :: a -> m a
  evalM = fixpointM stepM

  traceM :: Traversable m => a -> [m a]
  traceM = allPointsM stepM


-- | Successively apply @g@ to a given argument until the result doesn't change.
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint g = fix (\rec x -> if g x == x then x else rec (g x))

-- | Similar to `fixpoint` for functions that return monads. It also stops when
-- the result is `mzero`.
fixpointM :: (Eq (m a), Monad m) => (a -> m a) -> a -> m a
fixpointM g = fix (\rec x -> if g x == return x then return x else rec =<< g x)

-- | Returns a list of repeated applications of @f@ to @x@ stopping when the
-- fixpoint is reached.
allPoints :: Eq a => (a -> a) -> a -> [a]
allPoints g x = if g x == x then [x] else x:(allPoints g (g x))

-- | Similar to `allPoints` for functions that return monads. It also stops when
-- the result is `mzero`.
allPointsM :: (Eq (m a), Monad m, Traversable m) => (a -> m a) -> a -> [m a]
allPointsM g x = if g x == return x then [return x]
                                    else fmap join $ mapM (allPointsM g) (g x)

