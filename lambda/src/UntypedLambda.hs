{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-pattern-synonym-signatures -Wno-unused-do-bind #-}

{-# LANGUAGE FlexibleContexts, TupleSections, PatternSynonyms, ViewPatterns #-}

module UntypedLambda where

import Text.ParserCombinators.Parsec hiding (parse, State)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)

import Data.List ((\\))
import Data.Maybe (fromJust)

--------------------
-- Bisimulation
--------------------
--
--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alpha_A    alpha_B
--    |          |
--    |          |
--
--    A' --f'--> B'
--
--  A  - Abstract representation (E.g., List)        == Notional machine
--  A' - Concrete representation (E.g., ArrayList)   == Programming language
--  f  - Abstract program state transition function  == Notional machine "process"
--  f' - Concrete program state transition function (e.g. reduction)
--  alpha_X - Abstraction function
--
-- The abstraction is correct if:
-- alpha_B . f' == f . alpha_A
--------------------

--------------------
-- Untyped Lambda Calculus
--------------------
type Program = Exp
data Exp = App Exp Exp
         | Lambda Name Exp
         | Var Name
         deriving (Show, Read, Eq, Ord)
type Name = String

--------------------
-- Interpreter for Untyped Lambda Calculus
--------------------
eval :: Program -> Program
eval = bigStep

step :: Program -> Program
step (App      (Lambda name e1) e2 @ (Lambda _ _)) = subst name e2 e1
step (App e1 @ (Lambda _    _ ) e2               ) = App e1 (step e2)
step (App e1                    e2               ) = App (step e1) e2
step p @ (Lambda _ _) = p
step p @ (Var _) = p

bigStep :: Program -> Program
bigStep = fixChangeThat step

-- successively apply f to x until the result doesn't change
fixChangeThat :: Eq a => (a -> a) -> a -> a
fixChangeThat g x | g x == x  = x
                  | otherwise = fixChangeThat g (g x)

-- substitution
subst :: Name -> Exp -> Exp -> Exp
subst x v (App e1 e2)    = App (subst x v e1) (subst x v e2)
subst x v e  @ (Var y)
  | x == y               = v
  | otherwise            = e
subst x v e1 @ (Lambda y e2)
  | x == y               = e1
  | y `notElem` freeVs v = Lambda y    (subst x v e2                     )
  | otherwise            = Lambda newy (subst x v (subst y (Var newy) e2))
  where newy = fresh y

freeVs :: Exp -> [Name]
freeVs (Var name) = [name]
freeVs (Lambda name e) = freeVs e \\ [name]
freeVs (App e1 e2) = freeVs e1 ++ freeVs e2

fresh :: Name -> Name
fresh a = "_" ++ a


----- Evaluation with error handling ----------

evalMaybe :: Program -> Maybe Program
evalMaybe (App e1 e2) = do
  Lambda name e3 <- evalMaybe e1
  e4             <- evalMaybe e2
  evalMaybe (subst name e4 e3)
evalMaybe p @ (Lambda _ _) = Just p
evalMaybe (Var _) = Nothing -- "malformed exp tree"

stepMaybe :: Program -> Maybe Program
stepMaybe (App      (Lambda name e1) e2 @ (Lambda _ _)) = Just (subst name e2 e1)
stepMaybe (App e1 @ (Lambda _    _ ) e2               ) = do newe <- stepMaybe e2
                                                             return (App e1 newe)
stepMaybe (App e1                    e2               ) = do newe <- stepMaybe e1
                                                             return (App newe e2)
stepMaybe p @ (Lambda _ _) = Just p
stepMaybe (Var _) = Nothing

bigStepMaybe :: Program -> Maybe Program
bigStepMaybe = fixM stepMaybe

-- successively apply f to x until the result doesn't change
fixM :: (Monad m, Eq (m a)) => (a -> m a) -> a -> m a
fixM g x | g x == return x = return x
         | otherwise       = fixM g =<< g x


--------------------
-- Parsing and unparsing
--------------------
parse :: String -> Maybe Exp
parse   = parseMeta (pMetaLang pULCVarDcl)

parseJS :: String -> Maybe Exp
parseJS = parseMeta (pMetaLang pJSVarDcl)

parseMeta :: Parser Exp -> String -> Maybe Exp
parseMeta parser s = case Parsec.parse parser "(unknown)" s of
                       Left _ -> Nothing
                       Right e -> Just e

pMetaLang :: (Parser String -> Parser String) -> Parser Exp
pMetaLang pVarDcl = pExp <* eof
  where pExp = try pLambda <|> try pApp <|> pAtom
        pLambda = Lambda <$> pVarDcl pName <*> pExp
        pName = many1 (letter <|> char '_')
        pApp = foldl1 App <$> pAtom `sepBy1` spaces
        pAtom = pVar <|> between (char '(') (char ')') pExp
        pVar = Var <$> pName

pULCVarDcl :: Parser String -> Parser String
pULCVarDcl pName = between (char '\\') (char '.') pName

pJSVarDcl :: Parser String -> Parser String
pJSVarDcl pName = pName *> between spaces spaces (string "=>")

-----

unparse :: Program -> String
unparse (App e1 e2 @ (App _ _)) = unwords [unparse e1, parens (unparse e2)]
unparse (App e1 e2)     = unwords [unparse e1, unparse e2]
unparse (Lambda name e) = parens (concat ["\\", name, ".", unparse e])
unparse (Var name)      = name

parens :: String -> String
parens x = "(" ++ x ++ ")"

unparseJS :: Program -> String
unparseJS p @ (App _ _)       = unparse p
unparseJS     (Lambda name e) = parens (concat [name, " => ", unparse e])
unparseJS p @ (Var _)         = unparse p


--------------
-- Examples --
--------------
tru    :: String
tru    = "(\\t.\\f.t)"
eTrue  :: Exp
eTrue  = fromJust $ parse tru
fls    :: String
fls    = "(\\t.\\f.f)"
eFalse :: Exp
eFalse = fromJust $ parse fls
eAnd   :: Exp
eAnd   = fromJust $ parse ("\\b.\\c.b c " ++ fls)
eOr    :: Exp
eOr    = fromJust $ parse ("\\b.\\c.b " ++ tru ++ " c")
eZero  :: Exp
eZero  = fromJust $ parse "\\s.\\z.z"
eOne   :: Exp
eOne   = fromJust $ parse "\\s.\\z.s z"
eTwo   :: Exp
eTwo   = fromJust $ parse "\\s.\\z.s(s z)"
eScc   :: Exp
eScc   = fromJust $ parse "\\n.\\s.\\z.s(n s z)"
eOmega :: Exp
eOmega = fromJust $ parse "(\\x.x x) (\\x.x x)"
eY     :: Exp
eY     = fromJust $ parse "\\f.(\\x.f (x x)) (\\x.f (x x))"

