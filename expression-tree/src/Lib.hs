{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-pattern-synonym-signatures -Wno-unused-do-bind #-}

{-# LANGUAGE TupleSections, PatternSynonyms, ViewPatterns #-}

module Lib where

import Text.ParserCombinators.Parsec hiding (parse, State)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)

import Control.Monad.State.Lazy

import Data.List ((\\), uncons)

import Data.Set (Set)
import qualified Data.Set as Set

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
-- Expression Tree Diagram
--------------------
data ExpTreeDiagram = ExpTreeDiagram { nodes :: Set Node
                                     , edges :: Set Edge
                                     , root  :: Maybe Node } deriving (Show, Eq)
data Node = Node { nodePlug :: Plug
                 , content  :: [Fragment] } deriving (Show, Eq, Ord)
data Fragment = Token String
              | FragName String
              | Hole Plug
              deriving (Show, Eq, Ord)
data Plug = Plug Int Int deriving (Show, Eq, Ord)
data Edge = Edge Plug Plug deriving (Show)

-- the graph is undirected
instance Eq Edge where
  (Edge p1 p2) == (Edge p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
instance Ord Edge where
  compare (Edge p1 p2) (Edge p3 p4) = compare (Set.fromList [p1,p2]) (Set.fromList [p3,p4])

holes :: Node -> [Plug]
holes (Node _ fragments) = [plug | Hole plug <- fragments]

--------------------
-- Untyped Lambda Calculus
--------------------
type Program = Exp
data Exp = App Exp Exp
         | Lambda Name Exp
         | Var Name
         deriving (Show, Read, Eq, Ord)
type Name = String
type Env = [(Name, Exp)]

--------------------
-- Interpreter for Untyped Lambda Calculus
--------------------
eval :: Program -> Maybe Program
eval (App e1 e2) = do
  Lambda name e3 <- eval e1
  e4             <- eval e2
  eval (subst name e4 e3)
eval p @ (Lambda _ _) = Just p
eval (Var _) = Nothing -- "malformed exp tree"

step :: Program -> Maybe Program
step (App      (Lambda name e1) e2 @ (Lambda _ _)) = Just (subst name e2 e1)
step (App e1 @ (Lambda _    _ ) e2               ) = do newe <- step e2
                                                        return (App e1 newe)
step (App e1                    e2               ) = do newe <- step e1
                                                        return (App newe e2)
step p @ (Lambda _ _) = Just p
step (Var _) = Nothing

bigStep :: Program -> Maybe Program
bigStep = fixM step

-- successively apply f to x until the result doesn't change
fixM :: (Monad m, Eq (m a)) => (a -> m a) -> a -> m a
fixM g x | g x == return x = return x
         | otherwise       = fixM g =<< g x

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

--------------------
-- Parsing and unparsing
--------------------
parse :: String -> Maybe Program
parse s = case Parsec.parse pProg "(unknown)" s of
            Left _ -> Nothing
            Right e -> Just e
  where pProg = pExp <* eof
        pExp = pLambda
           <|> try pApp
           <|> pAtom
        pAtom = pVar
            <|> pParens pExp
        pVar = Var <$> pName
        pLambda = do char '\\'
                     var <- pName
                     char '.'
                     e <- pExp
                     return $ Lambda var e
        pApp = foldl1 App <$> pAtom `sepBy1` spaces
        pName = many1 (letter <|> char '_')
        pParens = between (char '(') (char ')')

unparse :: Program -> String
unparse (App e1 e2)     = parens (unwords [unparse e1, unparse e2])
unparse (Lambda name e) = parens (concat ["\\", name, ".", unparse e])
unparse (Var name)      = name

parens :: String -> String
parens x = "(" ++ x ++ ")"

--------------------
-- AST to Graph and back
--------------------

pattern NodeVar    i name <- Node (Plug i _) [FragName name] where
        NodeVar    i name =  Node (Plug i 0) [FragName name]
pattern NodeLambda i name <- Node (Plug i _) [Token "lambda", FragName name, Hole _] where
        NodeLambda i name =  Node (Plug i 0) [Token "lambda", FragName name, Hole (Plug i 1)]
pattern NodeApp    i      <- Node (Plug i _) [Hole _         , Hole _] where
        NodeApp    i      =  Node (Plug i 0) [Hole (Plug i 1), Hole (Plug i 2)]

pattern DiaLeaf :: Node -> ExpTreeDiagram
pattern DiaLeaf n <- ExpTreeDiagram _ _ (Just n @ (NodeVar _ _)) where
  DiaLeaf n = ExpTreeDiagram (Set.singleton n) Set.empty (Just n)

pattern DiaBranch :: Node -> [ExpTreeDiagram] -> ExpTreeDiagram
pattern DiaBranch r ns <- (diaBranch -> Just (r, ns)) where
  DiaBranch n = foldl merge (DiaLeaf n)
    where
      merge :: ExpTreeDiagram -> ExpTreeDiagram -> ExpTreeDiagram
      merge d1 @ (ExpTreeDiagram ns1 es1 r1) (ExpTreeDiagram ns2 es2 r2) =
        ExpTreeDiagram (Set.union ns1 ns2) (Set.unions [es1, es2, newEdge]) (mplus r1 r2)
        where -- create a singleton set with an edge connecting both roots if possible
              newEdge = maybe Set.empty Set.singleton (join $ mkEdge d1 <$> r1 <*> r2)
              -- Make an edge between the next available hole of n1 and the plug of n2
              mkEdge d n1 n2 = Edge (nodePlug n2) <$> maybeHead (emptyHoles d n1)
              -- plugs from a node that are not present in any edge
              emptyHoles d = filter (\p -> all (not . inEdge p) (edges d)) . holes
              inEdge p (Edge p1 p2) = p1 == p || p2 == p

-- returns the root node and a list of diagrams rooted at its children
diaBranch :: ExpTreeDiagram -> Maybe (Node, [ExpTreeDiagram])
diaBranch d = (\r -> (r, children r)) <$> root d
  where
    children n = [d { root = Just node } | node <- Set.elems (nodes d), node `isChild` n]
    isChild (Node nPlug _) = any (\p -> Set.member (Edge nPlug p) (edges d)) . holes

maybeHead :: [a] -> Maybe a
maybeHead = fmap fst . uncons


ast2diagram :: Program -> ExpTreeDiagram
ast2diagram = a2g 0
  where a2g uid (Var name)      = DiaLeaf   (NodeVar    uid name)
        a2g uid (Lambda name e) = DiaBranch (NodeLambda uid name) [a2g (uid + 1) e]
        a2g uid (App e1 e2)     = let d1 = a2g (uid      + 1) e1
                                      d2 = a2g (maxId d1 + 1) e2
                                  in DiaBranch (NodeApp uid) [d1, d2]

        maxId = foldl (\m (Node (Plug n _) _) -> max m n) 0 . nodes


diagram2ast :: ExpTreeDiagram -> Maybe Program
diagram2ast d = evalStateT (g2a d) Set.empty
  where
    -- traverse diagram to build Exp keeping track of visited nodes to not get stuck
    g2a :: ExpTreeDiagram -> StateT (Set Int) Maybe Program
    g2a (DiaLeaf   (NodeVar    i name))          = ifNotVisited i (return (Var name))
    g2a (DiaBranch (NodeLambda i name) [n])      = ifNotVisited i (Lambda name <$> g2a n)
    g2a (DiaBranch (NodeApp    i)      [n1, n2]) = ifNotVisited i (App <$> g2a n1 <*> g2a n2)
    g2a _ = StateT (const Nothing) -- "incorrect diagram"

    -- if `i` was not visited, add it to the state and perform `a` (Nothing otherwise)
    ifNotVisited i a = do visited <- get
                          if Set.member i visited then StateT (const Nothing)
                                                  else withStateT (Set.insert i) a


------------------

--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alphaA    alphaB
--    |          |
--    |          |
--
--    A' --f'--> B'

type A' = String
type B' = Maybe String

type A  = Maybe ExpTreeDiagram
type B  = Maybe ExpTreeDiagram

f' :: String -> Maybe String
f' = fmap unparse . (=<<) eval . parse

alphaA :: String -> Maybe ExpTreeDiagram
alphaA = fmap ast2diagram . parse

f :: Maybe ExpTreeDiagram -> Maybe ExpTreeDiagram
f = fmap ast2diagram . (=<<) eval . (=<<) diagram2ast

alphaB :: Maybe String -> Maybe ExpTreeDiagram
alphaB = (=<<) alphaA


-- Commutation proof:
-- alpha_B . f' == f . alpha_A

alphaBf' :: A' -> B
-- alpha_B__f' = alpha_B . f'
-- alpha_B__f' = (=<<) (fmap ast2diagram . parse) . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2diagram . parse) . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2diagram) . fmap parse . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2diagram) . fmap (parse . unparse) . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2diagram) . fmap return . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2diagram) . return . (=<<) eval . parse
alphaBf' = fmap ast2diagram . (=<<) eval . parse

falphaA :: A' -> B
-- f__alpha_A = f . alpha_A
-- f__alpha_A = fmap ast2diagram . (=<<) eval . ((=<<) diagram2ast) . fmap ast2diagram . parse
-- f__alpha_A = fmap ast2diagram . (=<<) eval . join . fmap diagram2ast . fmap ast2diagram . parse
-- f__alpha_A = fmap ast2diagram . (=<<) eval . join . fmap (diagram2ast . ast2diagram) . parse
-- f__alpha_A = fmap ast2diagram . (=<<) eval . join . fmap return . parse
-- f__alpha_A = fmap ast2diagram . (=<<) eval . join . return . parse
falphaA = fmap ast2diagram . (=<<) eval . parse




------------------
-- Expression Tutor activities
------------------

---- Parse activity ----

-- generateParseActivity = ... in the tests ...

solveParseActivity :: String -> Maybe ExpTreeDiagram
solveParseActivity = fmap ast2diagram . parse


---- Unparse activity ----

-- generateUnparseActivity = ... in the tests ...

solveUnparseActivity :: ExpTreeDiagram -> Maybe String
solveUnparseActivity = fmap unparse . diagram2ast


---- Eval activity ----

-- generateEvalActivity = ... in the tests ...

solveEvalActivity :: String -> Maybe String
solveEvalActivity = fmap unparse . (=<<) eval . parse




--------
-- TODO
-- - hedgehog: timeout to try to find hanging code
-- - hedgehog: config tree max depth
--
-- - eval done by labeling instead of rewrite
--
-- - generate diagram directly (not from exp) - closer to real-world cases
--
-- - unit testing with specific examples
-- - code coverage of hedgehog tests
--
-- - capture errors with Either
--
-- Finally, how much time would it take to extend your untyped lambda calculus – expression trees formalization to include alligator eggs as a second visualization? I reread Victor’s write up (see Teams posts to you yesterday night for links), and I think it would be a neat and geeky alternative NM to ET, and it would demonstrate a different kind of reusability of your model/framework/methodology: developing a new NM given an existing PL (and, in your specific current formalization, AST/intermediate layer). Bret Victor and his NM are well known and respected, so it would be an attractive demonstration of your tool framework. As I posted in Teams, there already are at least two interactive web-based alligator egg implementations (one includes parse, both include evaluate). However, they are for demonstration only: they don’t allow mistakes, and only one allows some kind of editing of the NM representation. If you spit out the data structure needed for these visualizations, then someone could maybe develop a complete UI that also allows one to make mistakes. While this is not on the critical path of your research, I think modeling the alligator eggs NM inside your existing framework, and spitting out something that could be used to generate the visualizations, would be worth a day or two of your time. Is this doable in that amount of time?


-- Degrees of freedom:
-- - different lang (lambda calculus, BSL) (delta in the impl?, do we abstract?)
-- - different NM (rewrite, labeling)
-- - different activities (parse, unparse, eval, type-check)
--
--
-- ✗ eval returns a value: failed
--   after 57 tests.
--
--   stack overflow
--
--   This failure can be reproduced by running:
--   > recheck (Size 56) (Seed 16402291810627727854 9477610571844893389) eval returns a value:
--
--
-- ✗ eval is equivalent to bigStep: failed
--   after 26 tests.
--
--   stack overflow
--
--   This failure can be reproduced by running:
--   > recheck (Size 25) (Seed 11466113076951511145 2415080421448164445) eval is equivalent to bigStep:
-- 
-- 2021-05-19:
