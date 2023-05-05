{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Description : Typing relation for Untyped Arithmetic Expressions from TAPL Ch.8
Stability   : experimental

Besides the `typeof` function, this module provides an instance of `StepabbleM`
which typechecks the term and only evaluates it if it's type-safe.
-}

module NotionalMachines.Lang.TypedArith.Main (
  Type(..),
  typeof,

  TypedTerm,
  typedTerm,
  TyTerm(..),
  typeof1,
  typeofEnd,

  Error(..),
  parse,

  repl
  ) where

import Data.Bifunctor (first)

import Prettyprinter (Pretty, hsep, pretty, (<+>))

import           NotionalMachines.Lang.Error             (Error (..), mismatch, typeOfEq)
import           NotionalMachines.Lang.UntypedArith.Main hiding (parse, repl)
import qualified NotionalMachines.Lang.UntypedArith.Main as Untyped

import NotionalMachines.Meta.Steppable (SteppableM, eval, step, stepM, trace)

import NotionalMachines.Util.REPL (LangPipeline (LangPipeline), mkLangRepl, taplBookMsg)


data Type = TyBool | TyNat
  deriving (Eq, Show)

instance Pretty Type where
  pretty = \case
    TyBool -> "Bool"
    TyNat  -> "Nat"

typeof :: Term -> Either Error Type
typeof e = case e of
  Tru         -> return TyBool                         -- T-True
  Fls         -> return TyBool                         -- T-False
  If t1 t2 t3 -> do typ1 <- typeof t1
                    typ2 <- typeof t2
                    _    <- typeof t3
                    case typ1 of
                      TyBool -> typeOfEq' t3 typ2 typ2 -- T-If
                      _      -> mismatch' TyBool typ1 t1
  Zero        -> return TyNat                          -- T-Zero
  Succ t      -> typeOfEq' t TyNat TyNat               -- T-Succ
  Pred t      -> typeOfEq' t TyNat TyNat               -- T-Pred
  IsZero t    -> typeOfEq' t TyNat TyBool              -- T-IsZero
  where typeOfEq' = typeOfEq typeof e
        mismatch' = mismatch e

----------------
----------------

type TypedTerm = (TyTerm, Maybe Type)

emptyTypedTerm :: TyTerm -> TypedTerm
emptyTypedTerm t = (t, Nothing)

data TyTerm -- Booleans
            = TyTru
            | TyFls
            | TyIf TypedTerm TypedTerm TypedTerm
            -- Arithmetic Expressions
            | TyZero
            | TySucc TypedTerm
            | TyPred TypedTerm
            | TyIsZero TypedTerm
  deriving (Eq, Show)

typedTerm :: Term -> TypedTerm
typedTerm = \case If e1 e2 e3 -> emptyTypedTerm $ TyIf     (typedTerm e1) (typedTerm e2) (typedTerm e3)
                  Succ t      -> emptyTypedTerm $ TySucc   (typedTerm t)
                  Pred t      -> emptyTypedTerm $ TyPred   (typedTerm t)
                  IsZero t    -> emptyTypedTerm $ TyIsZero (typedTerm t)
                  Tru         -> emptyTypedTerm TyTru
                  Fls         -> emptyTypedTerm TyFls
                  Zero        -> emptyTypedTerm TyZero

instance Pretty TyTerm where
  pretty = \case
    TyTru         -> "true"
    TyFls         -> "false"
    TyIf t1 t2 t3 -> hsep ["if", p t1, "then", p t2, "else", p t3]
    TyZero        -> "0"
    TySucc t      -> "succ"   <+> p t
    TyPred t      -> "pred"   <+> p t
    TyIsZero t    -> "iszero" <+> p t
    where
      p (term, Nothing) = pretty term
      p (term, Just _)  = pretty term -- use for debugin... parens (pretty t <> colon <+> pretty typ)

typeof1 :: TypedTerm -> Either Error TypedTerm
typeof1 (e, _) = case e of
  TyTru  -> return (e, Just TyBool) -- T-True
  TyFls  -> return (e, Just TyBool) -- T-False
  TyZero -> return (e, Just TyNat)  -- T-Zero

  TyIf (t1, Nothing) t2 t3 -> do tt <- typeof1 (t1, Nothing)
                                 return (TyIf tt t2 t3, Nothing)
  TyIf t1 (t2, Nothing) t3  -> do tt <- typeof1 (t2, Nothing)
                                  return (TyIf t1 tt t3, Nothing)
  TyIf t1 t2 (t3, Nothing)  -> do tt <- typeof1 (t3, Nothing)
                                  return (TyIf t1 t2 tt, Nothing)
  TyIf (_, Just TyBool) (_, Just typ2) (t3, Just typ3) | typ2 == typ3 -> return (e, Just typ2)
                                                       | typ2 /= typ3 -> mismatch' typ2 typ3 t3
  TyIf (t1, Just typ1) _ _ -> mismatch' TyBool typ1 t1

  TySucc t1@(_, Nothing)   -> do tt <- typeof1 t1              -- T-Succ
                                 return (TySucc tt, Nothing)   -- T-Succ
  TySucc (_, Just TyNat)   -> return (e, Just TyNat)           -- T-Succ
  TySucc (t, Just typ)     -> mismatch' TyNat typ t

  TyPred t1@(_, Nothing)   -> do tt <- typeof1 t1              -- T-Pred
                                 return (TyPred tt, Nothing)   -- T-Pred
  TyPred (_, Just TyNat)   -> return (e, Just TyNat)           -- T-Pred
  TyPred (t, Just typ)     -> mismatch' TyNat typ t

  TyIsZero t1@(_, Nothing) -> do tt <- typeof1 t1              -- T-IsZero
                                 return (TyIsZero tt, Nothing) -- T-IsZero
  TyIsZero (_, Just TyNat) -> return (e, Just TyBool)          -- T-IsZero
  TyIsZero (t, Just typ)   -> mismatch' TyNat typ t
  where mismatch' = mismatch e

typeofEnd :: Term -> Either Error Type
typeofEnd = go . typedTerm
  where go :: TypedTerm -> Either Error Type
        go (_, Just typ)  = return typ
        go t@(_, Nothing) = go =<< typeof1 t

----------------
----------------

instance SteppableM Term (Either Error) where
  stepM t = step t <$ typeof t

parse :: String -> Either Error Term
parse = first ParseError . Untyped.parse

--------------------
-- REPL
--------------------

langPipeline :: LangPipeline Term Type Error [Term]
langPipeline = LangPipeline parse (Right . eval) (Just typeof) (Right . trace)

repl :: IO ()
repl = mkLangRepl "TypedArith>" (taplBookMsg "8") langPipeline
