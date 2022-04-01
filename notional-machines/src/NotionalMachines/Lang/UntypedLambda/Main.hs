{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.Lang.UntypedLambda.Main where

import           Text.ParserCombinators.Parsec       (Parser, between, char, eof, letter, many1,
                                                      sepBy1, spaces, try, (<|>))
import qualified Text.ParserCombinators.Parsec       as Parsec (parse)
import           Text.ParserCombinators.Parsec.Error (ParseError)

import Prettyprinter (Pretty, backslash, dot, parens, pretty, (<+>))

import Data.List ((\\))

import NotionalMachines.Meta.Steppable (Steppable, SteppableM, eval, step, stepM, trace)
import NotionalMachines.Utils          (LangPipeline (LangPipeline), mkLangRepl, mkReplEval,
                                        prettyToString, taplBookMsg)

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
  deriving (Eq, Ord, Read, Show)
type Name = String

isValue :: Exp -> Bool
isValue Lambda {} = True
isValue _         = False

--------------------
-- Interpreter for Untyped Lambda Calculus
--------------------
instance Steppable Exp where
  step (App e1@(Lambda {}) e2@(App {})) = App e1 (step e2)
  step (App (Lambda name e1) e2)        = subst name e2 e1
  step (App e1 e2)                      = App (step e1) e2
  step p                                = p

-- substitution
subst :: Name -> Exp -> Exp -> Exp
subst x v (App e1 e2)    = App (subst x v e1) (subst x v e2)
subst x v e@(Var y)
  | x == y               = v
  | otherwise            = e
subst x v e1@(Lambda y e2)
  | x == y               = e1
  | y `notElem` freeVs v = Lambda y    (subst x v e2                     )
  | otherwise            = let newY = until (`notElem` freeVs v) fresh y
                            in Lambda newY (subst x v (subst y (Var newY) e2))

freeVs :: Exp -> [Name]
freeVs (Var name)      = [name]
freeVs (Lambda name e) = freeVs e \\ [name]
freeVs (App e1 e2)     = freeVs e1 ++ freeVs e2

fresh :: Name -> Name
fresh a = "_" ++ a


----- Evaluation with error handling ----------

instance SteppableM Exp Maybe where
  stepM (App    (Lambda name e1) e2@(Lambda {})) = Just (subst name e2 e1)
  stepM (App e1@(Lambda _    _ ) e2) = do newe <- stepM e2
                                          return (App e1 newe)
  stepM (App e1                  e2) = do newe <- stepM e1
                                          return (App newe e2)
  stepM p@(Lambda {}) = Just p
  stepM Var {} = Nothing

--------------------
-- Parsing and unparsing
--------------------
parse :: String -> Either ParseError Exp
parse = Parsec.parse (pExp <* eof) ""

pExp :: Parser Exp
pExp = try pLambda
   <|> try pApp
   <|> pAtom
  where
    pLambda =     Lambda <$> between (char '\\') (char '.' <* spaces) pName <*> pExp
    pApp    = foldl1 App <$> pAtom `sepBy1` spaces
    pVar    =        Var <$> pName
    pAtom = pVar <|> between (char '(') (char ')') pExp

    pName = many1 (letter <|> char '_')

-----

instance Pretty Exp where
  pretty = \case
    App e1 e2@(App {}) -> pretty e1 <+> parens (pretty e2)
    App e1 e2          -> pretty e1 <+>         pretty e2
    Lambda name e      -> parens (mconcat [backslash, pretty name, dot, pretty e])
    Var name           -> pretty name

unparse :: Exp -> String
unparse = prettyToString

--------------
-- Examples --
--------------

tru, fls, sAnd :: String
tru    = "(\\t.\\f.t)"
fls    = "(\\t.\\f.f)"
sAnd   = "\\b.\\c.b c " ++ fls

eId, eTrue, eFalse, eAnd, eOr, eZero, eOne, eTwo, eThree, eScc, eOmega, eY, eFix :: Either ParseError Exp

eId = parse "\\x.x"

-- church booleans
eTrue  = parse tru
eFalse = parse fls
eAnd   = parse sAnd
eOr    = parse ("\\b.\\c.b " ++ tru ++ " c")

-- church numbers
eZero  = parse "\\s.\\z.z"
eOne   = parse "\\s.\\z.s z"
eTwo   = parse "\\s.\\z.s(s z)"
eThree = parse "\\s.\\z.s(s (s z))"
eScc   = parse "\\n.\\s.\\z.s(n s z)"

-- recursion
eOmega = parse "(\\x.x x) (\\x.x x)"
eY     = parse "\\f.(\\x.f (x x)) (\\x.f (x x))"
eFix   = parse "\\f.(\\x.f (\\y.(x x) y)) (\\x.f (\\y.(x x) y))"

--------------------
-- REPL
--------------------

langPipeline :: LangPipeline Exp () ParseError [Exp]
langPipeline = LangPipeline parse (Right . eval) Nothing (Right . trace)

replEval :: String -> Either ParseError String
replEval = mkReplEval langPipeline

repl :: IO ()
repl = mkLangRepl "Lambda>" (taplBookMsg "5") langPipeline
