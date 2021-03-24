{-# LANGUAGE TupleSections #-}

module Lib where

import Text.Read (readMaybe)

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
data Fragment = Token String | FragName String | Whole deriving (Show, Eq)
data Edge = Edge Node Node deriving (Eq) -- the edge is directed from fst to snd

instance Show Edge where
  show (Edge (Node id1 _) (Node id2 _)) =
    unwords ["Edge", show id1, show id2]

-- instance Show Node where
--   show (Node uid fragments) =
--     unwords (fmap show fragments) ++ "(id=" ++ show uid ++ ")"

-- instance Show Fragment where
--   show (Token t) = t
--   show (FragName name) = name
--   show Whole = "#"

-- mkAppNode uid = Node uid [Whole, Whole]

-- isAppNode (Node _ [Whole, Whole]) = True
-- isAppNode _ = False

-- mkLambdaNode uid name = Node uid [Token "lambda", FragName name, Whole]

-- isLambdaNode (Node _ [Token "lambda", FragName _, Whole]) = True
-- isLambdaNode _ = False

-- mkVarNode uid name = Node uid [FragName name]

-- isVarNode (Node _ [FragName _]) = True
-- isVarNode _ = False

-- isValidNode node = or (fmap (\fun -> fun node) [isAppNode, isLambdaNode, isVarNode])

--------------------
-- Lambda calculus
type Program = (Exp, Env)
data Exp = App Exp Exp
         | Lambda Name Exp
         | Var Name
         | Closure Env Name Exp deriving (Show, Read, Eq, Ord)
type Name = String
type Env = [(Name, Exp)]
--------------------

initProgram :: Exp -> Program
initProgram expr = (expr, [])

eval :: Program -> Maybe Program
eval (Var name     , env)  = fmap (, env) (lookup name env)
eval (Lambda name e, env)  = Just (Closure env name e, env)
eval (App e1 e2    , env1) = do
  (Closure env2 name e3, _) <- eval (e1, env1)
  (e4, _)                   <- eval (e2, env1)
  eval (e3, (name, e4):env2)
eval (Closure {}, _)    = Nothing -- "malformed exp tree"

isValue :: Exp -> Bool
isValue Lambda {} = True
isValue Closure {} = True
isValue _ = False


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

        a2g uid (App e1 e2)        = mkBranch uid (Node uid [Whole, Whole]) [e1, e2]
        a2g uid (Lambda name e)    = mkBranch uid (Node uid [Token "lambda", FragName name, Whole]) [e]
        a2g uid (Var name)         = mkLeaf       (Node uid [FragName name])
        a2g uid (Closure _ name e) = a2g uid (Lambda name e)

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
    spanningTree (nodes, edges, curNode @ (Node _ [Whole, Whole])) =
      case diaNextNodes nodes edges curNode of
        [tuple1, tuple2] -> App <$> spanningTree tuple1 <*> spanningTree tuple2
        _ -> Nothing -- "incorrect diagram"
    spanningTree (nodes, edges, curNode @ (Node _ [Token "lambda", FragName name, Whole])) =
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
-- - small-step semantics


-- Degrees of freedom:
-- - different lang (lambda calculus, BSL) (delta in the impl?, do we abstract?)
-- - different NM (rewrite, labeling)
-- - different activities (parse, unparse, eval, type-check)