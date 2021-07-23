{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE LambdaCase #-}

{-|
Description : Typing relation for Untyped Arithmetic Expressions from TAPL Ch.8
Stability   : experimental

Besides the `typeof` function, this module provides an instance of `StepabbleM`
which typechecks the term and only evaluates it if it's type-safe.
-}

module NotionalMachines.Lang.TypedArith (
  typeof,
  Type(..)
  ) where

import NotionalMachines.Lang.Arith
import NotionalMachines.Meta.Steppable

data Type = TyBool | TyNat deriving (Eq, Show)

typeof :: Term -> Maybe Type
typeof = \case
  Tru                                  -> return TyBool -- T-True
  Fls                                  -> return TyBool -- T-False
  If t1 t2 t3 | typeof t1 == return TyBool
             && typeof t2 == typeof t3 -> typeof t2     -- T-If
  Zero                                 -> return TyNat  -- T-Zero
  Succ t   | typeof t == return TyNat  -> return TyNat  -- T-Pred
  Pred t   | typeof t == return TyNat  -> return TyNat  -- T-Succ
  IsZero t | typeof t == return TyNat  -> return TyBool -- T-IsZero
  _                                    -> Nothing

instance SteppableM Term where
  stepM t = const (step t) <$> typeof t
