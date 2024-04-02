{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTs #-}

module NotionalMachines.Machine.ListAsStackOfBoxes.Main where

newtype Box = Box String
  deriving (Show, Eq)

data Stack where
  -- A stack that is empty and only has the pallet.
  Pallet :: Stack
  -- Stacking a box on top of a stack.
  Stack :: Box -> Stack -> Stack
  deriving (Show, Eq)


-- TODO: write the fun below as a pattern

-- | When trying to pick up a box,
-- either there is only the pallet, in which case we get nothing,
-- or there is a box on top of a stack, in which case we get the box
-- and are left with the rest of the stack.
--
-- >>> pickUp (Stack (Box 1) Pallet)
-- Just (Box 1,Pallet)
-- >>> pickUp Pallet
-- Nothing
pickUp :: Stack -> Maybe (Box, Stack)
pickUp Pallet      = Nothing
pickUp (Stack b s) = Just (b, s)

