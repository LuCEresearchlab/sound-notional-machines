{-# LANGUAGE TupleSections #-}

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
---- Data Types ------
--------------------

--------------------
-- Expression     -- A', B'
type ExpText = String
type ExpressionEnv = (ExpText, Env)

--------------------
-- Expression Tree Diagram -- A , B
data ExpTreeDiagram = ExpTreeDia { diaNodes :: [Node]
                                 , diaEdges :: [Edge]
                                 , diaRoot  :: Maybe Node
                                 , diaEnv   :: Env } deriving (Show, Eq)
data Node = Node { nodeId  :: Int
                 , content :: [Fragment] } deriving (Show, Eq)
data Fragment = Token String | FragName String | Hole deriving (Show, Eq)
data Edge = Edge Node Node deriving (Eq) -- the edge is directed from fst to snd

instance Show Edge where
  show (Edge (Node id1 _) (Node id2 _)) =
    unwords ["Edge", show id1, show id2]

--------------------
-- Untyped Lambda Calculus
type Program = (Exp, Env)
data Exp = App Exp Exp
         | Lambda Name Exp
         | Var Name deriving (Show, Read, Eq, Ord)
type Name = String
type Env = [(Name, Exp)]

--------------------
-- Interpreter for Untyped Lambda Calculus
initProgram :: Exp -> Program
initProgram expr = (expr, [])

eval :: Program -> Maybe Program
eval (App e1 e2    , env) = do
  (Lambda name e3, _) <- eval (e1, env)
  (e4, _)             <- eval (e2, env)
  eval (subst name e4 e3, env)
eval p @ (Lambda _ _, _)  = Just p
eval (Var _, _) = Nothing -- "malformed exp tree"

step :: Program -> Maybe Program
step (App      (Lambda name e1) e2 @ (Lambda _ _), env) = Just (subst name e2 e1, env)
step (App e1 @ (Lambda _    _ ) e2               , env) = do (e3, _) <- eval (e2, env)
                                                             return (App e1 e3, env)
step (App e1 e2, env) = do (e3, _) <- eval (e1, env)
                           return (App e3 e2, env)
step p @ (Lambda _ _, _) = Just p
step (Var _, _) = Nothing

bigStep :: Program -> Maybe Program
bigStep = fixM step

-- successively apply f to x until the result doesn't change
fixM :: (Monad m, Eq (m a)) => (a -> m a) -> a -> m a
fixM g x | g x == return x = return x
         | otherwise       = fixM g =<< g x

subst :: Name -> Exp -> Exp -> Exp
subst x v      (App e1 e2)                          = App (subst x v e1) (subst x v e2)
subst x v  e @ (Var y)       | x == y               = v
                             | otherwise            = e
subst x v e1 @ (Lambda y e2) | x == y               = e1
                             | notElem y (freeVs v) = Lambda y    (subst x v e2                     )
                             | otherwise            = Lambda newy (subst x v (subst y (Var newy) e2))
  where newy = fresh y

freeVs :: Exp -> [Name]
freeVs (Var name) = [name]
freeVs (Lambda name e) = freeVs e \\ [name]
freeVs (App e1 e2) = freeVs e1 ++ freeVs e2

fresh :: Name -> Name
fresh a = "_" ++ a
--------------------

-- parse :: (ExpText, Env) -> (Term, Env)
parse :: ExpressionEnv -> Maybe Program
parse (e, env) = fmap (, env) (runParser e)
  where runParser = readMaybe

unparse :: Program -> ExpressionEnv
unparse (e, env) = (runPrettyPrinter e, env)
  where runPrettyPrinter = show


ast2graph :: Program -> ExpTreeDiagram
ast2graph (expr, env) = mkDiagram (a2g 0 expr)
  where mkDiagram (nodes, edges, root) = ExpTreeDia nodes edges (Just root) env

        a2g uid (App e1 e2)        = mkBranch uid (Node uid [Hole, Hole]) [e1, e2]
        a2g uid (Lambda name e)    = mkBranch uid (Node uid [Token "lambda", FragName name, Hole]) [e]
        a2g uid (Var name)         = mkLeaf       (Node uid [FragName name])

        mkLeaf node = ([node], [], node)
        mkBranch uid node exps =
          foldl (\tuple @ (nodes, _, _) e -> merge tuple (a2g (nextId uid nodes) e))
                (mkLeaf node)
                exps

        merge (nodes1, edges1, r1) (nodes2, edges2, r2) =
          (nodes1 ++ nodes2, edges1 ++ [Edge r1 r2] ++ edges2, r1)

        nextId = foldl (\m (Node n _) -> 1 + max m n)

graph2ast :: ExpTreeDiagram -> Maybe Program
graph2ast (ExpTreeDia _ _ Nothing _) = Nothing
graph2ast (ExpTreeDia ns es (Just r) env) = (, env) <$> spanningTree (ns, es, r)
  where
    spanningTree (_,     _,                Node _ [FragName name]) = Just (Var name)
    spanningTree (nodes, edges, curNode @ (Node _ [Hole, Hole])) =
      case diaNextNodes nodes edges curNode of
        [tuple1, tuple2] -> App <$> spanningTree tuple1 <*> spanningTree tuple2
        _ -> Nothing -- "incorrect diagram"
    spanningTree (nodes, edges, curNode @ (Node _ [Token "lambda", FragName name, Hole])) =
      case diaNextNodes nodes edges curNode of
        [tuple] -> Lambda name <$> spanningTree tuple
        _ -> Nothing -- "incorrect diagram"
    spanningTree _ = Nothing -- "incorrect diagram"

    diaNextNodes nodes edges root =
      [(nodes, edges, n2)| Edge n1 n2 <- edges, nodeId n1 == nodeId root]


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

type A' = ExpressionEnv
type B' = Maybe ExpressionEnv

type A  = Maybe ExpTreeDiagram
type B  = Maybe ExpTreeDiagram

f' :: ExpressionEnv -> Maybe ExpressionEnv
f' = fmap unparse . (=<<) eval . parse

alphaA :: ExpressionEnv -> Maybe ExpTreeDiagram
alphaA = fmap ast2graph . parse

f :: Maybe ExpTreeDiagram -> Maybe ExpTreeDiagram
f = fmap ast2graph . (=<<) eval . (=<<) graph2ast

alphaB :: Maybe ExpressionEnv -> Maybe ExpTreeDiagram
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

solveParseActivity :: ExpressionEnv -> Maybe ExpTreeDiagram
solveParseActivity = fmap ast2graph . parse


---- Unparse activity ----

-- generateUnparseActivity = ... in the tests ...

solveUnparseActivity :: ExpTreeDiagram -> Maybe String
solveUnparseActivity = fmap (fst . unparse) . graph2ast


---- Parse activity ----

-- generateEvalActivity = ... in the tests ...

solveEvalActivity :: ExpressionEnv -> Maybe String
solveEvalActivity = fmap (fst . unparse) . (=<<) eval . parse




--------
-- TODO
-- - hedgehog: timeout to try to find hanging code
-- - hedgehog: config tree max depth
--
-- - More realistic parse/unparse
--
-- - Use views to restrict kinds of valid Nodes (e.g. App, Var, Lambda)
-- - Change language to BSL like
--
-- - eval done by labeling instead of rewrite
--
-- - Edge should be from Node to Hole
-- - graph should contain sets (not lists) (shouldn't depend on the order) and see how far i can go


-- Degrees of freedom:
-- - different lang (lambda calculus, BSL) (delta in the impl?, do we abstract?)
-- - different NM (rewrite, labeling)
-- - different activities (parse, unparse, eval, type-check)
