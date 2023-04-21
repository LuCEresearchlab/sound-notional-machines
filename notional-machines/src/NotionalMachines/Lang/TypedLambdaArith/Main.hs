{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Description : Simply Typed Lambda Calculus with Booleans and Natural numbers based on TAPL Ch.9
Stability   : experimental

The simply typed lambda-calculus with booleans and natural numbers as base
types. The implementation follows the book "Types and Programming Languages",
Pierce (the TAPL book). The typing rules for the pure simply typed lambda
calculus are presented in chapter 9. The typing rules for booleans and
arithmetic expressions is presented in chapter 8. The evaluation rules for the
call-by-value lambda calculus are presented in chapter 5. The evaluation rules
for booleans and arithmetic expressions are presented in chapter 3.

-}

module NotionalMachines.Lang.TypedLambdaArith.Main (
  Term(..),
  Type(..),
  Error(..),
  isValue,
  typeof,
  typeof',
  parse,
  unparse,
  repl,
  replEval) where

import qualified Text.ParserCombinators.Parsec       as Parsec (parse)
import           Text.ParserCombinators.Parsec       hiding (parse)
import qualified Text.ParserCombinators.Parsec.Expr  as Ex
import qualified Text.ParserCombinators.Parsec.Token as Tok

import Prettyprinter (Pretty, hsep, parens, pretty, (<+>))

import Data.Bifunctor (first)
import Data.List      ((\\))

import NotionalMachines.Meta.Steppable (Steppable, eval, step, trace)
import NotionalMachines.Utils          (Error (..), LangPipeline (LangPipeline), maybeToEither,
                                        mismatch, mkLangRepl, mkReplEval, prettyToString,
                                        taplBookMsg, typeOfEq)

--------------------
-- Simply Typed Lambda Calculus + Booleans and Arithmetic Expressions
--------------------
data Type = TyFun Type Type
          | TyBool
          | TyNat
  deriving (Eq, Show)

type TypCtx = [(Name, Type)]

data Term -- lambdas
          = Var Name
          | Lambda Name Type Term
          | App Term Term
          -- Booleans
          | Tru
          | Fls
          | If Term Term Term
          -- Arithmetic Expressions
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
  deriving (Eq, Show)

type Name = String

typeof :: Term -> Either Error Type
typeof = typeof' []

typeof' :: TypCtx -> Term -> Either Error Type
typeof' ctx e = case e of
  Var name        -> maybeToEither
                     (TypeError $ "variable '" ++ name ++ "' not in scope.")
                     (lookup name ctx)                                  -- T-Var
  Lambda x typ1 t -> TyFun typ1 <$> typeof' ((x, typ1):ctx) t           -- T-Abs
  App t1 t2       -> do typ1 <- rec t1
                        case typ1 of
                          TyFun typ11 typ12 -> typeOfEq' t2 typ11 typ12 -- T-App
                          _ -> Left . TypeError $
                               "expected function type but found " ++ prettyToString typ1
  Tru                                             -> return TyBool      -- T-True
  Fls                                             -> return TyBool      -- T-False
  If t1 t2 t3     -> do typ1 <- rec t1
                        typ2 <- rec t2
                        case typ1 of
                          TyBool -> typeOfEq' t3 typ2 typ2              -- T-If
                          _      -> mismatch' TyBool typ1 t1
  Zero            -> return TyNat                                       -- T-Zero
  Succ t          -> typeOfEq' t TyNat TyNat                            -- T-Succ
  Pred t          -> typeOfEq' t TyNat TyNat                            -- T-Pred
  IsZero t        -> typeOfEq' t TyNat TyBool                           -- T-IsZero
  where rec = typeof' ctx
        typeOfEq' = typeOfEq rec e
        mismatch' = mismatch e

isValue :: Term -> Bool
isValue = \case
  Lambda {} -> True
  Tru       -> True
  Fls       -> True
  t         -> isNumericVal t

isNumericVal :: Term -> Bool
isNumericVal = \case
  Zero   -> True
  Succ t -> isNumericVal t
  _      -> False

instance Steppable Term where
  step = step'

step' :: Term -> Term
step' = \case
  App e1 e2 | not (isValue e1)     -> App (step' e1) e2   -- E-App1
  App v1 e2 | not (isValue e2)     -> App v1 (step' e2)   -- E-App2
  App (Lambda name _ e1) e2        -> subst name e2 e1    -- E-AppAbs
  If Tru t2 _                      -> t2                  -- E-IfTrue
  If Fls _  t3                     -> t3                  -- E-IfFalse
  If t1  t2 t3                     -> If (step' t1) t2 t3 -- E-If
  Succ t                           -> Succ (step' t)      -- E-Succ
  Pred Zero                        -> Zero                -- E-PredZero
  Pred (Succ t) | isNumericVal t   -> t                   -- E-PredSucc
  Pred t                           -> Pred (step' t)      -- E-Pred
  IsZero Zero                      -> Tru                 -- E-IsZeroZero
  IsZero (Succ t) | isNumericVal t -> Fls                 -- E-IsZeroSucc
  IsZero t                         -> IsZero (step' t)    -- E-IsZero
  t                                -> t

-- | Return @e@ with all free occurences of @x@ substituted by @v@a.
-- Renaming of variables is performed as need to avoid variable capture.
subst :: Name -> Term -> Term -> Term
subst x v e = case e of
  App t1 t2                             -> App (rec t1) (rec t2)
  Var y | x == y                        -> v
        | otherwise                     -> e
  Lambda y ty t2 | x == y               -> e
                 | y `notElem` freeVs v -> Lambda y ty (rec t2)
                 | otherwise            -> let newY = until (`notElem` freeVs v) fresh y
                                            in Lambda newY ty (rec (subst y (Var newY) t2))
  If t1 t2 t3                           -> If (rec t1) (rec t2) (rec t3)
  Succ t                                -> Succ (rec t)
  Pred t                                -> Pred (rec t)
  IsZero t                              -> IsZero (rec t)
  _                                     -> e
  where rec = subst x v

freeVs :: Term -> [Name]
freeVs = \case
  Var name        -> [name]
  Lambda name _ t -> freeVs t \\ [name]
  App t1 t2       -> freeVs t1 ++ freeVs t2
  If t1 t2 t3     -> freeVs t1 ++ freeVs t2 ++ freeVs t3
  Succ t          -> freeVs t
  Pred t          -> freeVs t
  IsZero t        -> freeVs t
  _               -> []

fresh :: Name -> Name
fresh a = "_" ++ a


--------------------
-- Parsing and unparsing
--------------------
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = ["if", "then", "else", "true", "false"]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

contents :: Parser a -> Parser a
contents p = do Tok.whiteSpace lexer
                p <* eof

pParens :: Parser a -> Parser a
pParens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

pTerm :: Parser Term
pTerm = Ex.buildExpressionParser table factor
  where table = [ [ prefixOp "succ" Succ
                  , prefixOp "pred" Pred
                  , prefixOp "iszero" IsZero ]
                , [ Ex.Infix (App <$ reservedOp "") Ex.AssocLeft ] ]
        prefixOp s f = Ex.Prefix (f <$ reservedOp s)

factor :: Parser Term
factor = Tru  <$ reserved   "true"
     <|> Fls  <$ reserved   "false"
     <|> Zero <$ reservedOp "0"
     <|> pIf
     <|> pLambda
     <|> pVar
     <|> pParens pTerm
  where
    pLambda = do { reservedOp "\\"; name <- identifier;
                   reservedOp ":";  typ  <- pTyp;
                   reservedOp ".";  body <- pTerm;
                   return $ Lambda name typ body }
    pIf = do { reserved "if";   c <- pTerm;
               reserved "then"; t <- pTerm;
               reserved "else"; f <- pTerm;
               return $ If c t f }
    pVar = Var <$> identifier

pTypAtom :: Parser Type
pTypAtom = TyBool <$ reserved "Bool"
       <|> TyNat  <$ reserved "Nat"
       <|> pParens pTyp

pTyp :: Parser Type
pTyp = Ex.buildExpressionParser table pTypAtom
  where table = [ [ Ex.Infix (TyFun <$ reservedOp "->") Ex.AssocRight ] ]

parse :: String -> Either Error Term
parse = first ParseError . Parsec.parse (contents pTerm) ""

-----

instance Pretty Term where
  pretty = \case
    App e1@(If {}) e2@(App {}) -> parens (pretty e1) <+> parens (pretty e2)
    App e1@(If {}) e2@(If {})  -> parens (pretty e1) <+> parens (pretty e2)
    App e1@(If {}) e2          -> parens (pretty e1) <+>         pretty e2
    App e1         e2@(If {})  ->         pretty e1  <+> parens (pretty e2)
    App e1         e2@(App {}) ->         pretty e1  <+> parens (pretty e2)
    App e1         e2          ->         pretty e1  <+>         pretty e2
    Lambda name typ e          -> parens (mconcat ["\\", pretty name, ":", pretty typ, ".", pretty e])
    Var name                   -> pretty name
    Tru                        -> "true"
    Fls                        -> "false"
    If t1 t2 t3@(App {})       -> hsep ["if", pretty t1, "then", pretty t2, "else", parens (pretty t3)]
    If t1 t2 t3                -> hsep ["if", pretty t1, "then", pretty t2, "else", pretty t3]
    Zero                       -> "0"
    Succ t                     -> "succ"   <+> parens (pretty t)
    Pred t                     -> "pred"   <+> parens (pretty t)
    IsZero t                   -> "iszero" <+> parens (pretty t)

instance Pretty Type where
  pretty = \case
    TyBool                 -> "Bool"
    TyNat                  -> "Nat"
    TyFun t1@(TyFun {}) t2 -> mconcat [parens (pretty t1), "->", pretty t2]
    TyFun t1 t2            -> mconcat [        pretty t1,  "->", pretty t2]

unparse :: Term -> String
unparse = prettyToString

--------------------
-- REPL
--------------------

langPipeline :: LangPipeline Term Type Error [Term]
langPipeline = LangPipeline parse (Right . eval) (Just typeof) (Right . trace)

replEval :: String -> Either Error String
replEval = mkReplEval langPipeline

repl :: IO ()
repl = mkLangRepl "TypedLambda>" (taplBookMsg "9") langPipeline
