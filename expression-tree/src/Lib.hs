{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-pattern-synonym-signatures #-}

{-# LANGUAGE TupleSections, PatternSynonyms, ViewPatterns #-}

module Lib where

import Text.Read (readMaybe)
import Data.List ((\\))

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
--       A  - Abstract representation (E.g., abstract data structure: List)      == Notional machine
--       A' - Concrete representation (E.g., concrete data structure: ArrayList) == Programming language
--       f  - Abstract program  state transition function                        == Notional machine "process"
--       f' - Concrete program state transition function (e.g. reduction)
--  alpha_X - Abstraction function
--
-- The abstraction is correct if:
-- alpha_B . f' == f . alpha_A
--------------------

--------------------
-- Expression Tree Diagram
--------------------
data ExpTreeDiagram = ExpTreeDia { diaNodes :: [Node]
                                 , diaEdges :: [Edge]
                                 , diaRoot  :: Maybe Node
                                 , diaEnv   :: Env } deriving (Show, Eq)
data Node = Node { nodeId  :: Int
                 , content :: [Fragment] } deriving (Show, Eq)
data Fragment = Token String
              | FragName String
              | Hole
              deriving (Show, Eq)
data Edge = Edge Node Node deriving (Eq) -- the edge is directed from fst to snd

pattern NodeVar    uid name = Node uid [FragName name]
pattern NodeLambda uid name = Node uid [Token "lambda", FragName name, Hole]
pattern NodeApp    uid      = Node uid [Hole, Hole]

instance Show Edge where
  show (Edge (Node id1 _) (Node id2 _)) =
    unwords ["Edge", show id1, show id2]

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
subst x v      (App e1 e2)                          = App (subst x v e1) (subst x v e2)
subst x v  e @ (Var y) | x == y                     = v
                       | otherwise                  = e
subst x v e1 @ (Lambda y e2) | x == y               = e1
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
parse = readMaybe

unparse :: Program -> String
unparse = show

--------------------
-- AST to Graph and back
--------------------

-- rooted (right) diagram
data RDia = RDia [Node] [Edge] Node Env

rooted2dia :: RDia -> ExpTreeDiagram
rooted2dia (RDia nodes edges root env) = ExpTreeDia nodes edges (Just root) env

dia2rooted :: ExpTreeDiagram -> Maybe RDia
dia2rooted (ExpTreeDia nodes edges (Just root) env) = Just (RDia nodes edges root env)
dia2rooted ExpTreeDia {diaRoot = Nothing} = Nothing

pattern DiaLeaf :: Node -> RDia
pattern DiaLeaf n <- RDia _ _ n @ (NodeVar _ _) _ where
  DiaLeaf n = RDia [n] [] n []

pattern DiaBranch :: Node -> [RDia] -> RDia
pattern DiaBranch r ns <- (diaNextNodes -> (RDia _ _ r _):ns) where
  DiaBranch n = foldl merge (DiaLeaf n)
    where merge (RDia nodes1 edges1 r1 env1) (RDia nodes2 edges2 r2 env2) =
            RDia (nodes1 ++ nodes2) (edges1 ++ [Edge r1 r2] ++ edges2) r1 (env1 ++ env2)

diaNextNodes :: RDia -> [RDia]
diaNextNodes dia @ (RDia nodes edges root env) =
  dia : [RDia nodes edges n2 env | Edge n1 n2 <- edges, nodeId n1 == nodeId root]


ast2graph :: Program -> ExpTreeDiagram
ast2graph expr = rooted2dia (a2g 0 expr)
  where a2g uid (Var name)      = DiaLeaf   (NodeVar    uid name)
        a2g uid (Lambda name e) = DiaBranch (NodeLambda uid name) [a2g (uid + 1) e]
        a2g uid (App e1 e2)     = let d1 = a2g (uid      + 1) e1
                                      d2 = a2g (maxId d1 + 1) e2
                                  in DiaBranch (NodeApp uid) [d1, d2]

        maxId (RDia nodes _ _ _) = foldl (\m (Node n _) -> max m n) 0 nodes

graph2ast :: ExpTreeDiagram -> Maybe Program
graph2ast = (=<<) spanningTree . dia2rooted
  where spanningTree (DiaLeaf   (NodeVar    _ name))          = Just (Var name)
        spanningTree (DiaBranch (NodeLambda _ name) [n])      = Lambda name <$> spanningTree n
        spanningTree (DiaBranch (NodeApp _)         [n1, n2]) = App <$> spanningTree n1 <*> spanningTree n2
        spanningTree _ = Nothing -- "incorrect diagram"


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
alphaA = fmap ast2graph . parse

f :: Maybe ExpTreeDiagram -> Maybe ExpTreeDiagram
f = fmap ast2graph . (=<<) eval . (=<<) graph2ast

alphaB :: Maybe String -> Maybe ExpTreeDiagram
alphaB = (=<<) alphaA


-- Commutation proof:
-- alpha_B . f' == f . alpha_A

alphaBf' :: A' -> B
-- alpha_B__f' = alpha_B . f'
-- alpha_B__f' = (=<<) (fmap ast2graph . parse) . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph . parse) . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . fmap parse . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . fmap (parse . unparse) . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . fmap return . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . return . (=<<) eval . parse
alphaBf' = fmap ast2graph . (=<<) eval . parse

falphaA :: A' -> B
-- f__alpha_A = f . alpha_A
-- f__alpha_A = fmap ast2graph . (=<<) eval . ((=<<) graph2ast) . fmap ast2graph . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . fmap graph2ast . fmap ast2graph . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . fmap (graph2ast . ast2graph) . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . fmap return . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . return . parse
falphaA = fmap ast2graph . (=<<) eval . parse




------------------
-- Expression Tutor activities
------------------

---- Parse activity ----

-- generateParseActivity = ... in the tests ...

solveParseActivity :: String -> Maybe ExpTreeDiagram
solveParseActivity = fmap ast2graph . parse


---- Unparse activity ----

-- generateUnparseActivity = ... in the tests ...

solveUnparseActivity :: ExpTreeDiagram -> Maybe String
solveUnparseActivity = fmap unparse . graph2ast


---- Eval activity ----

-- generateEvalActivity = ... in the tests ...

solveEvalActivity :: String -> Maybe String
solveEvalActivity = fmap unparse . (=<<) eval . parse




--------
-- TODO
-- - hedgehog: timeout to try to find hanging code
-- - hedgehog: config tree max depth
--
-- - More realistic parse/unparse
--
-- - eval done by labeling instead of rewrite
--
-- - Edge should be from Node to Hole
-- - graph should contain sets (not lists) (shouldn't depend on the order) and see how far i can go
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
