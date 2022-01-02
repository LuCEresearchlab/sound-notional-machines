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

import Data.Bifunctor (first)

import Prettyprinter (Pretty, pretty)

import           NotionalMachines.Lang.UntypedArith.Main (Term (..))
import qualified NotionalMachines.Lang.UntypedArith.Main as Untyped
import           NotionalMachines.Meta.Steppable         (SteppableM, eval, step, stepM, trace)
import           NotionalMachines.Utils                  (Error (..), LangPipeline (LangPipeline),
                                                          mkLangRepl, taplBookMsg, typeOfEq, mismatch)

data Type = TyBool | TyNat deriving (Eq, Show)

typeof :: Term -> Either Error Type
typeof e = case e of
  Tru         -> return TyBool                         -- T-True
  Fls         -> return TyBool                         -- T-False
  If t1 t2 t3 -> do typ1 <- typeof t1
                    typ2 <- typeof t2
                    case typ1 of
                      TyBool -> typeOfEq' t3 typ2 typ2 -- T-If
                      _      -> mismatch' TyBool typ1 t1
  Zero        -> return TyNat                          -- T-Zero
  Succ t      -> typeOfEq' t TyNat TyNat               -- T-Succ
  Pred t      -> typeOfEq' t TyNat TyNat               -- T-Pred
  IsZero t    -> typeOfEq' t TyNat TyBool              -- T-IsZero
  where typeOfEq' = typeOfEq typeof e
        mismatch' = mismatch e

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

langPipeline :: LangPipeline Term Type Error [Term]
langPipeline = LangPipeline parse (Right . eval) (Just typeof) (Right . trace)

repl :: IO ()
repl = mkLangRepl "TypedArith>" (taplBookMsg "8") langPipeline
