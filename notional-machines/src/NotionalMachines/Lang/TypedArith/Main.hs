{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

{-|
Description : Typing relation for Untyped Arithmetic Expressions from TAPL Ch.8
Stability   : experimental

Besides the `typeof` function, this module provides an instance of `StepabbleM`
which typechecks the term and only evaluates it if it's type-safe.
-}

module NotionalMachines.Lang.TypedArith.Main (
  Type(..),
  typeof,

  Error(..),
  parse,

  repl
  ) where

import Data.Text.Prettyprint.Doc (Pretty, pretty)

import Text.ParserCombinators.Parsec (ParseError)

import           Data.Bifunctor                          (first)
import           NotionalMachines.Lang.UntypedArith.Main (Term (..))
import qualified NotionalMachines.Lang.UntypedArith.Main as Untyped
import           NotionalMachines.Meta.Steppable         (SteppableM, eval, step, stepM, trace)
import           NotionalMachines.Utils                  (mkLangRepl, taplBookMsg)

data Type = TyBool | TyNat deriving (Eq, Show)
data Error = TypeError
           | ParseError ParseError
  deriving (Eq, Show)

typeof :: Term -> Either Error Type
typeof = \case
  Tru                                  -> return TyBool -- T-True
  Fls                                  -> return TyBool -- T-False
  If t1 t2 t3 | typeof t1 == return TyBool
             && typeof t2 == typeof t3 -> typeof t2     -- T-If
  Zero                                 -> return TyNat  -- T-Zero
  Succ t   | typeof t == return TyNat  -> return TyNat  -- T-Pred
  Pred t   | typeof t == return TyNat  -> return TyNat  -- T-Succ
  IsZero t | typeof t == return TyNat  -> return TyBool -- T-IsZero
  _                                    -> Left TypeError

instance SteppableM Term (Either Error) where
  stepM t = step t <$ typeof t

instance Pretty Type where
  pretty = \case
    TyBool -> pretty "Bool"
    TyNat  -> pretty "Nat"

parse :: String -> Either Error Term
parse = first ParseError . Untyped.parse

--------------------
-- REPL
--------------------

repl :: IO ()
repl = mkLangRepl "TypedArith>"
                  parse
                  (Right . eval)
                  (Right . trace)
                  (Just typeof)
                  (taplBookMsg "8")
