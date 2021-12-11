{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (
  parse,
  unparse
  ) where

import Data.Bifunctor (first)

import           Text.ParserCombinators.Parsec       hiding (parse)
import qualified Text.ParserCombinators.Parsec       as Parsec (parse)
import qualified Text.ParserCombinators.Parsec.Expr  as Ex
import qualified Text.ParserCombinators.Parsec.Token as Tok


import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Term (..), Type (..))
import NotionalMachines.Utils                              (Error (..), prettyToString)


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
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~;"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~;"
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

decimal :: Parser Integer
decimal = Tok.natural lexer

pTerm :: Parser Term
pTerm = Ex.buildExpressionParser table factor
  where table = [ [ Ex.Prefix (Deref  <$ reservedOp "!") ]
                , [ Ex.Prefix (Succ   <$ reserved   "succ")
                  , Ex.Prefix (Pred   <$ reserved   "pred")
                  , Ex.Prefix (IsZero <$ reserved   "iszero")
                  , Ex.Prefix (Ref    <$ reserved   "ref") ]
                , [ Ex.Infix  (App    <$ reservedOp "")       Ex.AssocLeft  ]
                , [ Ex.Infix  (Assign <$ reservedOp ":=")     Ex.AssocRight ]
                , [ Ex.Infix  (Seq    <$ reservedOp ";")      Ex.AssocRight ] ]

factor :: Parser Term
factor = Tru  <$ reserved "true"
     <|> Fls  <$ reserved "false"
     <|> Unit <$ reserved "unit"
     <|> Zero <$ reserved "0"
     <|> pIf
     <|> pLambda
     <|> Var <$> identifier
     <|> decToPeano <$> decimal
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

pTypAtom :: Parser Type
pTypAtom = TyBool <$ reserved "Bool"
       <|> TyNat  <$ reserved "Nat"
       <|> TyUnit <$ reserved "Unit"
       <|> pParens pTyp

pTyp :: Parser Type
pTyp = Ex.buildExpressionParser table pTypAtom
  where table = [ [ Ex.Infix  (TyFun <$ reservedOp "->")  Ex.AssocRight
                  , Ex.Prefix (TyRef <$ reservedOp "Ref") ] ]

parse :: String -> Either Error Term
parse = first ParseError . Parsec.parse (contents pTerm) ""

decToPeano :: Integer -> Term
decToPeano 0 = Zero
decToPeano n = Succ (decToPeano (n - 1))

unparse :: Term -> String
unparse = prettyToString


----

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

