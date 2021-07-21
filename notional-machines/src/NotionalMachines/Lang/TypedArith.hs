{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE LambdaCase, ViewPatterns #-}

{-|
Description : Typing relation for Untyped Arithmetic Expressions from TAPL Ch.8
Stability   : experimental

Besides the `typeof` function, this module provides an instance of `StepabbleM`
which typechecks the term and only evaluates it if it's type-safe.
-}

module NotionalMachines.Lang.TypedArith (
  typeof,
  Typ(..)
  ) where

import NotionalMachines.Lang.Arith
import NotionalMachines.Meta.Steppable

data Typ = TyBool | TyNat deriving (Eq, Show)

typeof :: Term -> Maybe Typ
typeof = \case
  Tru                                -> return TyBool -- T-True
  Fls                                -> return TyBool -- T-False
  If (typeof -> Just TyBool)
     (typeof -> typ2)
     (typeof -> typ3) | typ2 == typ3 -> typ2    --    -- T-If
  Zero                               -> return TyNat  -- T-Zero
  Succ   (typeof -> Just TyNat)      -> return TyNat  -- T-Pred
  Pred   (typeof -> Just TyNat)      -> return TyNat  -- T-Succ
  IsZero (typeof -> Just TyNat)      -> return TyBool -- T-IsZero
  _                                  -> Nothing

instance SteppableM Term where
  stepM t = const (step t) <$> typeof t
