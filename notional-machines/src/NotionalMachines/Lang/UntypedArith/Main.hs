{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : The Untyped Arithmetic Expressions from TAPL Ch.3
Stability   : experimental

A language of booleans and numbers as described in the book "Types and Programming Languages", Pierce, Chapter 3.

Reduction is done by implementing an instance of `Steppable` so you can do one step of reduction with:

>>> step (If (IsZero (Succ Zero)) Fls Tru)
If Fls Fls Tru

Evaluate an expression with:

>>> eval (If (IsZero (Succ Zero)) Fls Tru)
Tru

See all intermediate steps like so:

>>> Text.Show.Pretty.pPrintList $ allSteps (If (IsZero (Succ Zero)) Fls Tru)
[ If (IsZero (Succ Zero)) Fls Tru
, If Fls Fls Tru
, Tru
]

-}

module NotionalMachines.Lang.UntypedArith.Main (
  Term(..),
  isValue,
  parse,
  unparse
  ) where

import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

import Data.Text.Prettyprint.Doc (Pretty, hsep, pretty, (<+>))

import Data.Functor                    (($>))
import NotionalMachines.Meta.Steppable (Steppable, step)
import NotionalMachines.Utils          (eitherToMaybe, pShow)

data Term = -- Booleans
            Tru
          | Fls
          | If Term Term Term
            -- Arithmetic Expressions
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
  deriving (Eq, Show)

isValue :: Term -> Bool
isValue Tru = True
isValue Fls = True
isValue t   = isNumericVal t

isNumericVal :: Term -> Bool
isNumericVal Zero     = True
isNumericVal (Succ t) = isNumericVal t
isNumericVal _        = False

instance Steppable Term where
  step = \case
    If Tru t2 _                      -> t2                 -- E-IfTrue
    If Fls _  t3                     -> t3                 -- E-IfFalse
    If t1  t2 t3                     -> If (step t1) t2 t3 -- E-If
    Succ t                           -> Succ (step t)      -- E-Succ
    Pred Zero                        -> Zero               -- E-PredZero
    Pred (Succ t) | isNumericVal t   -> t                  -- E-PredSucc
    Pred t                           -> Pred (step t)      -- E-Pred
    IsZero Zero                      -> Tru                -- E-IsZeroZero
    IsZero (Succ t) | isNumericVal t -> Fls                -- E-IsZeroSucc
    IsZero t                         -> IsZero (step t)    -- E-IsZero
    t                                -> t

instance Pretty Term where
  pretty = \case
    Tru         -> "true"
    Fls         -> "false"
    If t1 t2 t3 -> hsep ["if", pretty t1, "then", pretty t2, "else", pretty t3]
    Zero        -> "0"
    Succ t      -> "succ"   <+> pretty t
    Pred t      -> "pred"   <+> pretty t
    IsZero t    -> "iszero" <+> pretty t

--------------------
-- Parsing and unparsing
--------------------
parse :: String -> Maybe Term
parse = eitherToMaybe . P.parse pTerm "(unknown)"

pTerm :: Parser Term
pTerm = string "true"  $> Tru
    <|> string "false" $> Fls
    <|> char '0'       $> Zero
    <|> try (If <$> (string "if" *> spaces *> pTerm)
                <*> (between spaces spaces (string "then") >> pTerm)
                <*> (between spaces spaces (string "else") >> pTerm))
    <|> Succ   <$> (string "succ"   *> spaces *> pTerm)
    <|> Pred   <$> (string "pred"   *> spaces *> pTerm)
    <|> IsZero <$> (string "iszero" *> spaces *> pTerm)
    <|> between (char '(') (char ')') pTerm
    <* eof

unparse :: Term -> String
unparse = pShow
