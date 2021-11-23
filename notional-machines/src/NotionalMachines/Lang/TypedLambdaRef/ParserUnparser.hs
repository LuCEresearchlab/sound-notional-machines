{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module NotionalMachines.Lang.TypedLambdaRef.ParserUnparser (
  parse,
  unparse
  ) where

import           Data.Bifunctor (first)
import qualified Data.Map       as Map

import           Text.ParserCombinators.Parsec       hiding (parse)
import qualified Text.ParserCombinators.Parsec       as Parsec (parse)
import qualified Text.ParserCombinators.Parsec.Expr  as Ex
import qualified Text.ParserCombinators.Parsec.Token as Tok

import Data.Text.Prettyprint.Doc (Doc, Pretty, align, concatWith, hardline, hsep, parens, pretty,
                                  (<+>))

import NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (Location, NameEnv, Store, Term (..),
                                                            Type (..), isNumVal)
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
                , [ Ex.Infix  (mkSeq  <$ reservedOp ";")      Ex.AssocRight ] ]
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

peanoToDec :: Term -> Integer
peanoToDec Zero     = 0
peanoToDec (Succ n) = succ (peanoToDec n)
peanoToDec t        = error $ "internal error: can't show term as number: " ++ show t
-----

instance Pretty Term where
  pretty = \case
    App (Lambda "$u" TyUnit t2) t1 -> mconcat [pretty t1, "; ", pretty t2]
    App e1 e2                      -> p e1 <+> p e2
    Lambda x t e                   -> parens (mconcat ["\\", pretty x, ":", pretty t, ". ", pretty e])
    Closure env x t                -> parens (mconcat ["Closure ", pretty env, " \\", pretty x, ". ", pretty t])
    Var x                          -> pretty x
    Unit                           -> "unit"
    Ref t                          -> "ref" <+> p t
    Deref t                        -> "!"   <>  p t
    Assign t1 t2                   -> hsep [p t1, ":=", pretty t2]
    Loc l                          -> "Loc" <+> pretty l
    Tru                            -> "true"
    Fls                            -> "false"
    If t1 t2 t3                    -> hsep ["if", pretty t1, "then", pretty t2, "else", p t3]
    Zero                           -> "0"
    Succ t | isNumVal t            -> pretty (peanoToDec (Succ t))
    Succ t | otherwise             -> "succ"   <+> p t
    Pred t                         -> "pred"   <+> p t
    IsZero t                       -> "iszero" <+> p t
    where p = parenIf $ \case App    {}                 -> True
                              If     {}                 -> True
                              Succ t | not (isNumVal t) -> True
                              Pred   {}                 -> True
                              IsZero {}                 -> True
                              Ref    {}                 -> True
                              Deref  {}                 -> True
                              Assign {}                 -> True
                              Loc    {}                 -> True
                              _                         -> False

instance Pretty Type where
  pretty = \case
    TyBool      -> "Bool"
    TyNat       -> "Nat"
    TyUnit      -> "Unit"
    TyRef t     -> "Ref" <+> p t
    TyFun t1 t2 -> mconcat [p t1, "->", pretty t2]
    where p = parenIf $ \case TyFun {} -> True
                              TyRef {} -> True
                              _        -> False

parenIf :: Pretty a => (a -> Bool) -> a -> Doc b
parenIf f t = (if f t then parens else id) (pretty t)

unparse :: Term -> String
unparse = prettyToString


------

instance Pretty (Store Location) where
  pretty m = "Store:" <+> pretty (Map.toList m)

instance Pretty NameEnv where
  pretty m = "NameEnv:" <+> align (hvsep (fmap pretty (Map.toList m)))
    where hvsep = concatWith (\x y -> x <> hardline <> y)

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

