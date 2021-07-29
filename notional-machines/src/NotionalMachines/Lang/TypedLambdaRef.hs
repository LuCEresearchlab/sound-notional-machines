{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

{-|
Description : Simply Typed Lambda Calculus with References, Unit, Booleans, and Natural numbers based on TAPL Ch.3 (WIP)
Stability   : experimental

WIP...

-}

module NotionalMachines.Lang.TypedLambdaRef (
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

import Data.Text.Prettyprint.Doc (Pretty, pretty, parens, (<+>), hsep)

import Data.Bifunctor (first)
import Data.List ((\\))

import NotionalMachines.Utils (maybeToEither, pShow)
import NotionalMachines.Meta.Steppable (SteppableM, stepM)

--------------------
-- Simply Typed Lambda Calculus + Unit + References + Booleans and Arithmetic Expressions
--------------------
data Type = TyFun Type Type
          | TyUnit
          | TyBool
          | TyNat
          deriving (Eq, Show)

type TypCtx = [(Name, Type)]

data Term = -- lambdas
            Var Name
          | Lambda Name Type Term
          | App Term Term
            -- Unit
          | Unit
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

isValue :: Term -> Bool
isValue = \case
  Lambda {} -> True
  Unit      -> True
  Tru       -> True
  Fls       -> True
  t         -> isNumericVal t

isNumericVal :: Term -> Bool
isNumericVal = \case
  Zero   -> True
  Succ t -> isNumericVal t
  _      -> False

typeof :: Term -> Either String Type
typeof = maybeToEither "type error" . typeof' []

typeof' :: TypCtx -> Term -> Maybe Type
typeof' ctx = \case
  Var name                                        -> lookup name ctx     -- T-Var
  Lambda x typ1 (typeof' ((x, typ1):ctx) -> typ2) -> TyFun typ1 <$> typ2 -- T-Abs
  App (typeof' ctx -> Just (TyFun typ11 typ12))
      (typeof' ctx -> Just typ2) | typ11 == typ2  -> return typ12        -- T-App
  Unit                                            -> return TyUnit       -- T-Unit
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

instance SteppableM Term (Either String) where
  stepM t = const (step' t) <$> typeof t

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
  , Tok.reservedNames   = ["if", "then", "else", "true", "false", "succ",
                           "pred", "iszero", "unit", "Bool", "Nat"]
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
  where table = [ [ Ex.Prefix (Succ   <$ reserved   "succ")
                  , Ex.Prefix (Pred   <$ reserved   "pred")
                  , Ex.Prefix (IsZero <$ reserved   "iszero")
                  , Ex.Infix  (App    <$ reservedOp "")       Ex.AssocLeft
                  , Ex.Infix  (mkSeq  <$ reservedOp ";")      Ex.AssocLeft ] ]
        -- Sequencing is a derived form (i.e. syntactic sugar). "$u" is always
        -- fresh (different from all the free vars in t2 because user-defined
        -- vars can't start with "$".
        mkSeq t1 t2 = App (Lambda "$u" TyUnit t2) t1

factor :: Parser Term
factor = Tru  <$ reserved "true"
     <|> Fls  <$ reserved "false"
     <|> Unit <$ reserved "unit"
     <|> Zero <$ reserved "0"
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
       <|> TyUnit <$ reserved "Unit"
       <|> pParens pTyp

pTyp :: Parser Type
pTyp = Ex.buildExpressionParser table pTypAtom
  where table = [ [ Ex.Infix (TyFun <$ reservedOp "->") Ex.AssocRight ] ]

parse :: String -> Either String Term
parse = first show . Parsec.parse (contents pTerm) "(unknown)"

-----

instance Pretty Term where
  pretty = \case
    App e1 e2         -> p e1 <+> p e2
    Lambda name typ e -> parens (mconcat ["\\", pretty name, ":", pretty typ, ". ", pretty e])
    Var name          -> pretty name
    Unit              -> "unit"
    Tru               -> "true"
    Fls               -> "false"
    If t1 t2 t3       -> hsep ["if", pretty t1, "then", pretty t2, "else", p t3]
    Zero              -> "0"
    Succ t            -> "succ"   <+> p t
    Pred t            -> "pred"   <+> p t
    IsZero t          -> "iszero" <+> p t
    where p t = (if isAtomic t then id else parens) (pretty t)
          isAtomic = \case
            Var {}    -> True
            Lambda {} -> True -- lambda is not atomic but is always parenthesized
            Unit      -> True
            Tru       -> True
            Fls       -> True
            Zero      -> True
            _         -> False


instance Pretty Type where
  pretty = \case
    TyBool                 -> "Bool"
    TyNat                  -> "Nat"
    TyUnit                 -> "Unit"
    TyFun t1 @ TyFun {} t2 -> mconcat [parens (pretty t1), "->", pretty t2]
    TyFun t1 t2            -> mconcat [        pretty t1,  "->", pretty t2]

unparse :: Term -> String
unparse = pShow


{-
Interesting insight about Unit and sequencing (TAPL Ch.11):

Should sequencing be left-associative or right-associative? Does it matter?
If we associate to the left, we get a lambda term made of applications
associated to the right and if we associate the sequence to the right we get a
lambda term associated to the left. Either wait the result seems to be the same
so does that mean the sequencing on terms is a Monoid with unit as the neutral
element? What are the consequences of this?

t1;t2;t3

t4;t3, t4 = t1;t2
(\\x:Unit.t3) t4
(\\x:Unit.t3) ((\\x:Unit.t2) t1)

t1;t5, t5 = t2;t3
(\\x:Unit.t5) t1
(\\x:Unit.((\\x:Unit.t3) t2)) t1
-}
