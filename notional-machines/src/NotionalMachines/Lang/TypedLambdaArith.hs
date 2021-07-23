{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, ViewPatterns #-}

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

module NotionalMachines.Lang.TypedLambdaArith (
  Term(..),
  Type(..),
  isValue,
  typeof,
  typeof',
  parse,
  unparse
  ) where

import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Bifunctor (first)
import Data.List ((\\))

import NotionalMachines.Utils (maybeToEither, eitherToMaybe)
import NotionalMachines.Meta.Steppable

--------------------
-- Simply Typed Lambda Calculus + Booleans and Arithmetic Expressions
--------------------
data Type = TyFun Type Type
          | TyBool
          | TyNat
          deriving (Eq, Show)

type TypCtx = [(Name, Type)]

data Term = -- lambdas
            Var Name
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

typeof :: Term -> Either String Type
typeof = maybeToEither "type error" . typeof' []

typeof' :: TypCtx -> Term -> Maybe Type
typeof' ctx = \case
  Var name                                        -> lookup name ctx     -- T-Var
  Lambda x typ1 (typeof' ((x, typ1):ctx) -> typ2) -> TyFun typ1 <$> typ2 -- T-Abs
  App (typeof' ctx -> Just (TyFun typ11 typ12))
      (typeof' ctx -> Just typ2) | typ11 == typ2  -> return typ12        -- T-App
  Tru                                             -> return TyBool       -- T-True
  Fls                                             -> return TyBool       -- T-False
  If t1 t2 t3 | typeOfEq t1 TyBool
             && typeof' ctx t2 == typeof' ctx t3  -> typeof' ctx t2
  Zero                                            -> return TyNat        -- T-Zero
  Succ t   | typeOfEq t TyNat                     -> return TyNat        -- T-Pred
  Pred t   | typeOfEq t TyNat                     -> return TyNat        -- T-Succ
  IsZero t | typeOfEq t TyNat                     -> return TyBool       -- T-IsZero
  _                                               -> Nothing
  where typeOfEq t1 t2 = typeof' ctx t1 == Just t2

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

instance SteppableM Term where
  stepM t = const (step' t) <$> eitherToMaybe (typeof t)

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

-- Substitute a name by a term in a second term returning the second term with
-- all occurences of the name replaced by the first term. Renaming of variables
-- is performed as need to avoid variable capture.
subst :: Name -> Term -> Term -> Term
subst x v (App e1 e2)    = App (subst x v e1) (subst x v e2)
subst x v e  @ (Var y)
  | x == y               = v
  | otherwise            = e
subst x v e1 @ (Lambda y t e2)
  | x == y               = e1
  | y `notElem` freeVs v = Lambda y    t (subst x v e2)
  | otherwise            = Lambda newy t (subst x v (subst y (Var newy) e2))
  where newy = fresh y
subst _ _ t              = t

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

-- TODO: i think this is incorrect. it doesn't guarantee a global fresh name.
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
                  , prefixOp "iszero" IsZero
                  , Ex.Infix (App <$ reservedOp "") Ex.AssocLeft ] ]
        prefixOp s f = Ex.Prefix (f <$ reservedOp s)

factor :: Parser Term
factor = Tru  <$ reserved   "true"
     <|> Fls  <$ reserved   "false"
     <|> Zero <$ reservedOp "0"
     <|> pIf
     <|> pLambda
     -- <|> pApp
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

parse :: String -> Either String Term
parse = first show . Parsec.parse (contents pTerm) "(unknown)"

-----

unparse :: Term -> String
unparse = \case
  App e1 @ If {} e2 @ App {} -> unwords [parens (unparse e1), parens (unparse e2)]
  App e1 @ If {} e2 @ If {}  -> unwords [parens (unparse e1), parens (unparse e2)]
  App e1 @ If {} e2          -> unwords [parens (unparse e1),         unparse e2]
  App e1         e2 @ If {}  -> unwords [        unparse e1,  parens (unparse e2)]
  App e1         e2 @ App {} -> unwords [        unparse e1,  parens (unparse e2)]
  App e1         e2          -> unwords [        unparse e1,          unparse e2]
  Lambda name typ e          -> parens (concat ["\\", name, ":", unparseTyp typ, ".", unparse e])
  Var name                   -> name
  Tru                        -> "true"
  Fls                        -> "false"
  If t1 t2 t3 @ App {}       -> unwords ["if", unparse t1, "then", unparse t2, "else", parens (unparse t3)]
  If t1 t2 t3                -> unwords ["if", unparse t1, "then", unparse t2, "else", unparse t3]
  Zero                       -> "0"
  Succ t                     -> unwords ["succ", parens (unparse t)]
  Pred t                     -> unwords ["pred", parens (unparse t)]
  IsZero t                   -> unwords ["iszero", parens (unparse t)]

unparseTyp :: Type -> String
unparseTyp = \case
  TyBool -> "Bool"
  TyNat  -> "Nat"
  TyFun t1 @ TyFun {} t2 -> concat [parens (unparseTyp t1), "->", unparseTyp t2]
  TyFun t1 t2            -> concat [        unparseTyp t1,  "->", unparseTyp t2]

parens :: String -> String
parens x = "(" ++ x ++ ")"

