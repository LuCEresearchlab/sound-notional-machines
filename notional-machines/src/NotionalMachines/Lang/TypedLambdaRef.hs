{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

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
  unparse,

  evalM',
  evalRaw,
  trace
  ) where

import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Text.Prettyprint.Doc (Pretty, pretty, parens, (<+>), hsep, Doc)

import Control.Monad.State.Lazy (StateT, get, withStateT, lift, evalStateT, runStateT, (<=<))

import Data.Bifunctor (first)
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map

import NotionalMachines.Utils (maybeToEither, pShow)
import NotionalMachines.Meta.Steppable (SteppableM, stepM, evalM, traceM)

--------------------
-- Simply Typed Lambda Calculus + Unit + References + Booleans and Arithmetic Expressions
--------------------
data Type = TyFun Type Type
          | TyUnit
          | TyRef Type
          | TyBool
          | TyNat
          deriving (Eq, Show)

type TypCtx = [(Name, Type)]

-- type TyStore = Map Location Type

data Term = -- Lambdas
            Var Name
          | Lambda Name Type Term
          | App Term Term
            -- Unit
          | Unit
            -- References
          | Ref Term
          | Deref Term
          | Assign Term Term
          | Loc Location
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

type Location = Int

type Store = Map Location Term

emptyStore :: Store
emptyStore = Map.empty

isValue :: Term -> Bool
isValue = \case
  Lambda {} -> True
  Unit      -> True
  Loc {}    -> True
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

-- TODO: use ReaderT here
typeof' :: TypCtx -> Term -> Maybe Type
typeof' ctx = \case
  -- Lambdas
  Var name                                        -> lookup name ctx     -- T-Var
  Lambda x typ1 (typeof' ((x, typ1):ctx) -> typ2) -> TyFun typ1 <$> typ2 -- T-Abs
  App (rec -> Just (TyFun typ11 typ12))
      (rec -> Just typ2) | typ11 == typ2          -> return typ12        -- T-App
  -- Unit
  Unit                                            -> return TyUnit       -- T-Unit
  -- References
  Ref    (rec -> typ1)                            -> TyRef <$> typ1
  Deref  (rec -> Just (TyRef typ1))               -> return typ1
  Assign (rec -> Just (TyRef typ1))
         (rec -> Just typ2) | typ1 == typ2        -> return TyUnit
  -- Booleans
  Tru                                             -> return TyBool       -- T-True
  Fls                                             -> return TyBool       -- T-False
  If t1 t2 t3 | typeOfEq t1 TyBool
             && rec t2 == rec t3                  -> rec t2
  -- Arithmetic Expressions
  Zero                                            -> return TyNat        -- T-Zero
  Succ t   | typeOfEq t TyNat                     -> return TyNat        -- T-Pred
  Pred t   | typeOfEq t TyNat                     -> return TyNat        -- T-Succ
  IsZero t | typeOfEq t TyNat                     -> return TyBool       -- T-IsZero
  _                                               -> Nothing
  where typeOfEq t1 t2 = rec t1 == Just t2
        rec = typeof' ctx

typecheck :: Term -> Either String Term
typecheck t = const t <$> typeof t

instance SteppableM Term (StateT Store (Either String)) where
  stepM = step'

evalM' :: Term -> Either String Term
evalM' t = evalStateT (evalM t) emptyStore

step' :: Term -> StateT Store (Either String) Term
step' = \case
  -- Lambdas
  App t1 t2 | not (isValue t1)     -> (\t1' -> App t1' t2 )    <$> step' t1                   -- E-App1
  App v1 t2 | not (isValue t2)     -> (\t2' -> App v1  t2')    <$> step' t2                   -- E-App2
  App (Lambda name _ t1) t2        -> return $ subst name t2 t1                               -- E-AppAbs
  -- References
  Ref v | isValue v                -> do newLoc <- fmap (foldl max 0 . Map.keys) get
                                         withStateT (Map.insert newLoc v) (pure $ Loc newLoc) -- E-RefV
  Ref t | otherwise                -> Ref   <$> step' t                                       -- E-Ref
  Deref (Loc l)                    -> do t <- fmap (Map.lookup l) get
                                         lift $ maybeToEither "internal error: invalid ref" t -- E-DerefLoc
  Deref t                          -> Deref <$> step' t                                       -- E-Deref
  Assign (Loc l) v | isValue v     -> withStateT (Map.insert l v) (return Unit)
  Assign t1 t2 | not (isValue t1)  -> (\t1' -> Assign t1' t2 ) <$> step' t1                   -- E-Assign1
  Assign v1 t2 | otherwise         -> (\t2' -> Assign v1  t2') <$> step' t2                   -- E-Assign2
  -- -- Booleans
  If Tru t2 _                      -> return t2                                               -- E-IfTrue
  If Fls _  t3                     -> return t3                                               -- E-IfFalse
  If t1  t2 t3                     -> (\t1' -> If t1' t2 t3)   <$> step' t1                   -- E-If
  -- -- Arithmetic Expressions
  Succ t                           -> Succ   <$> step' t                                      -- E-Succ
  Pred Zero                        -> return Zero                                             -- E-PredZero
  Pred (Succ v) | isNumericVal v   -> return v                                                -- E-PredSucc
  Pred t                           -> Pred   <$> step' t                                      -- E-Pred
  IsZero Zero                      -> return Tru                                              -- E-IsZeroZero
  IsZero (Succ v) | isNumericVal v -> return Fls                                              -- E-IsZeroSucc
  IsZero t                         -> IsZero <$> step' t                                      -- E-IsZero
  t                                -> return t

-- Substitute a name by a term in a second term returning the second term with
-- all occurences of the name replaced by the first term. Renaming of variables
-- is performed as need to avoid variable capture.
subst :: Name -> Term -> Term -> Term
subst x v e = case e of
  App e1 e2                            -> App (rec e1) (rec e2)
  Var y | x == y                       -> v
        | otherwise                    -> e
  Lambda y t e2 | x == y               -> e
                | y `notElem` freeVs v -> Lambda y    t (rec e2)
                | otherwise            -> Lambda (fresh y) t (rec (subst y (Var (fresh y)) e2))
  If t1 t2 t3                          -> If (rec t1) (rec t2) (rec t3)
  Succ t                               -> Succ (rec t)
  Pred t                               -> Pred (rec t)
  IsZero t                             -> IsZero (rec t)
  Ref t                                -> Ref (rec t)
  Deref t                              -> Deref (rec t)
  Assign t1 t2                         -> Assign (rec t1) (rec t2)
  t                                    -> t
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
  Ref t           -> freeVs t
  Deref t         -> freeVs t
  Assign t1 t2    -> freeVs t1 ++ freeVs t2
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

parse :: String -> Either String Term
parse = first show . Parsec.parse (contents pTerm) "(unknown)"

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
    Succ t | isNumericVal t        -> pretty (peanoToDec (Succ t))
    Succ t | otherwise             -> "succ"   <+> p t
    Pred t                         -> "pred"   <+> p t
    IsZero t                       -> "iszero" <+> p t
    where p = parenIf $ \case App    {}                     -> True
                              If     {}                     -> True
                              Succ t | not (isNumericVal t) -> True
                              Pred   {}                     -> True
                              IsZero {}                     -> True
                              Ref    {}                     -> True
                              Deref  {}                     -> True
                              Assign {}                     -> True
                              Loc    {}                     -> True
                              _                             -> False

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
unparse = pShow

----

evalRaw :: String -> Either String Term
evalRaw = evalM' <=< typecheck <=< parse

trace :: String -> Either String [(String, Store)]
trace = fmap (fmap (first unparse))
      . (=<<) (
          mapM (flip runStateT emptyStore)
        . traceM
      )
      . (=<<) typecheck
      . parse 

-- This instance is required for tracing because it needs to compare StateTs.
instance Eq (StateT Store (Either String) Term) where
  s1 == s2 = evalStateT s1 emptyStore == evalStateT s2 emptyStore


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
