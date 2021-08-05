{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiParamTypeClasses, LambdaCase #-}

module NotionalMachines.Lang.UntypedLambda.Main where

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)

import Data.Text.Prettyprint.Doc (Pretty, pretty, dot, backslash, parens, (<+>))

import Data.List ((\\))
import Data.Maybe (fromJust)

import NotionalMachines.Meta.Steppable (Steppable, SteppableM, step, stepM)
import NotionalMachines.Utils (pShow)

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
data Exp = App Exp Exp
         | Lambda Name Exp
         | Var Name
         deriving (Show, Read, Eq, Ord)
type Name = String

--------------------
-- Interpreter for Untyped Lambda Calculus
--------------------
instance Steppable Exp where
  step (App (e1 @ Lambda {}) (e2 @ App {})) = App e1 (step e2)
  step (App (Lambda name e1) e2) = subst name e2 e1
  step (App e1 e2) = App (step e1) e2
  step p = p

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

-- TODO: i think this is incorrect. it doesn't guarantee a global fresh name.
fresh :: Name -> Name
fresh a = "_" ++ a

isValue :: Exp -> Bool
isValue Lambda {} = True
isValue _         = False


----- Evaluation with error handling ----------

instance SteppableM Exp Maybe where
  stepM (App      (Lambda name e1) e2 @ (Lambda {})) = Just (subst name e2 e1)
  stepM (App e1 @ (Lambda _    _ ) e2) = do newe <- stepM e2
                                            return (App e1 newe)
  stepM (App e1                    e2) = do newe <- stepM e1
                                            return (App newe e2)
  stepM p @ (Lambda {}) = Just p
  stepM (Var {}) = Nothing

--------------------
-- Parsing and unparsing
--------------------
parse :: String -> Maybe Exp
parse s = case Parsec.parse pExp "(unknown)" s of
            Left _ -> Nothing
            Right e -> Just e

pExp :: Parser Exp
pExp = try pLambda <|> try pApp <|> pAtom <* eof
  where
    pLambda = Lambda <$> between (char '\\') (char '.') pName <*> pExp
    pName = many1 (letter <|> char '_')
    pApp = foldl1 App <$> pAtom `sepBy1` spaces
    pAtom = pVar <|> between (char '(') (char ')') pExp
    pVar = Var <$> pName

-----

instance Pretty Exp where
  pretty = \case
    App e1 e2 @ App {} -> pretty e1 <+> parens (pretty e2)
    App e1 e2          -> pretty e1 <+>         pretty e2
    Lambda name e      -> parens (mconcat [backslash, pretty name, dot, pretty e])
    Var name           -> pretty name

unparse :: Exp -> String
unparse = pShow

--------------
-- Examples --
--------------

eId :: Exp
eId = fromJust $ parse "\\x.x"

-- church booleans
tru    :: String
tru    = "(\\t.\\f.t)"
eTrue  :: Exp
eTrue  = fromJust $ parse tru
fls    :: String
fls    = "(\\t.\\f.f)"
eFalse :: Exp
eFalse = fromJust $ parse fls
sAnd   :: String
sAnd   = ("\\b.\\c.b c " ++ fls)
eAnd   :: Exp
eAnd   = fromJust $ parse sAnd
eOr    :: Exp
eOr    = fromJust $ parse ("\\b.\\c.b " ++ tru ++ " c")

-- church numbers
eZero  :: Exp
eZero  = fromJust $ parse "\\s.\\z.z"
eOne   :: Exp
eOne   = fromJust $ parse "\\s.\\z.s z"
eTwo   :: Exp
eTwo   = fromJust $ parse "\\s.\\z.s(s z)"
eThree :: Exp
eThree = fromJust $ parse "\\s.\\z.s(s (s z)"
eScc   :: Exp
eScc   = fromJust $ parse "\\n.\\s.\\z.s(n s z)"

eOmega :: Exp
eOmega = fromJust $ parse "(\\x.x x) (\\x.x x)"
eY     :: Exp
eY     = fromJust $ parse "\\f.(\\x.f (x x)) (\\x.f (x x))"
eFix   :: Exp
eFix   = fromJust $ parse "\\f.(\\x.f (\\y.(x x) y)) (\\x.f (\\y.(x x) y))"

