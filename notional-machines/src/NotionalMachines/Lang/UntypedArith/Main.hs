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
  unparse,

  replEval,
  repl) where

import           Data.Text.Prettyprint.Doc     (Pretty, hsep, pretty, (<+>))
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (ParseError, Parser, between, char, eof, spaces,
                                                string, try, (<|>))

import NotionalMachines.Meta.Steppable (Steppable, eval, step, trace)
import NotionalMachines.Utils          (mkLangRepl, mkReplEval, pShow, taplBookMsg)

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

--------------------
-- Parsing and unparsing
--------------------
parse :: String -> Either ParseError Term
parse = P.parse (pTerm <* eof) "(unknown)"
  where
    pTerm :: Parser Term
    pTerm = Succ <$> (reserved "succ" *> pTerm)
        <|> Pred <$> (reserved "pred" *> pTerm)
        <|> try (IsZero <$> (reserved "iszero" *> pTerm))
        <|> Tru  <$ reserved "true"
        <|> Fls  <$ reserved "false"
        <|> Zero <$ reserved "0"
        <|> If <$> (reserved "if"   *> pTerm)
               <*> (reserved "then" *> pTerm)
               <*> (reserved "else" *> pTerm)
        <|> between (char '(') (char ')') pTerm <* spaces

    reserved :: String -> Parser ()
    reserved s = string s *> spaces

instance Pretty Term where
  pretty = \case
    Tru         -> "true"
    Fls         -> "false"
    If t1 t2 t3 -> hsep ["if", pretty t1, "then", pretty t2, "else", pretty t3]
    Zero        -> "0"
    Succ t      -> "succ"   <+> pretty t
    Pred t      -> "pred"   <+> pretty t
    IsZero t    -> "iszero" <+> pretty t

unparse :: Term -> String
unparse = pShow

--------------------
-- REPL
--------------------

replEval :: String -> Either ParseError String
replEval = mkReplEval parse
                      (Right . eval)
                      (Nothing :: Maybe (Term -> Either ParseError ()))

repl :: IO ()
repl = mkLangRepl "Arith>"
                  parse
                  (Right . eval)
                  (Nothing :: Maybe (Term -> Either ParseError Term)) -- not typed
                  [("trace", Right . trace)]
                  (taplBookMsg "3")
