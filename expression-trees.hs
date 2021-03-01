module ExpressionTree where

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
--
--       A  - Abstract representation (E.g., abstract data structure: List) == Notional machine
--
--       A' - Concrete representation (E.g., concrete data structure: ArrayList) == Programming language
--
--       f  - Abstract program == Notional machine "process"
--
--       f' - Program state transition function (e.g. reduction)
--
--  alpha_X - Abstraction Function
--
--
-- The abstraction is correct if:
-- alpha_B . f' == f . alpha_A

-----------------


--------------------
-- Expression     -- A', B'
--------------------
type ExpText = String
type Expression = (ExpText, Env)

--------------------
-- Expression Tree Diagram -- A , B
--------------------
data ExpTreeDiagram = ExpTreeDia { nodes :: [Node]
                                 , edges :: [Edge]
                                 , root  :: Maybe Node
                                 , env   :: Env }
data Node = Node { nodeId    :: Int
                 , typ   :: NodeType
                 , label :: String }
data NodeType = Appl | Varb | Numb | Lamb
type Edge = (Node, Node)

--------------------
-- AST
--------------------
type AST = (Exp, Env)
data Exp = App Name [Exp]
         | Var Name
         | Val Value deriving (Show, Read)
data Value = Num Int
           | Lambda Name Exp deriving (Show, Read)
data FunDef = Fun Name [Name] Exp
data Program = Program [FunDef] Exp
--------------------

-- Common types
type Name = String
type Env = [(Name, Value)]

-- appEnv :: (a -> b) -> (a, Env) -> (b, Env)
liftEnv :: Monad m => (a -> m b) -> (a, Env) -> m (b, Env)
liftEnv f (a, env) = fmap (flip (,) env) (f a)


-- parse :: (ExpText, Env) -> (Term, Env)
parse :: Expression -> Maybe AST
parse = liftEnv runParser
  where runParser = readMaybe

unparse :: AST -> Expression
unparse (exp, env) = (runPrettyPrinter exp, env)
  where runPrettyPrinter = show


ast2graph :: AST -> ExpTreeDiagram
ast2graph (exp, env) = a2g 0 exp
  where a2g id (App name exps) = mkBranch id (Node id Appl name) exps
        a2g id (Var name) = mkLeaf (Node id Varb name)
        a2g id (Val (Num i)) = mkLeaf (Node id Numb (show i))
        a2g id (Val (Lambda name e)) = mkBranch id (Node id Lamb name) [e]

        mkLeaf node = ExpTreeDia [node] [] (Just node) env
        mkBranch id node exps = 
          foldl (\dia exp -> merge dia (a2g (nextId id (nodes dia)) exp))
                (mkLeaf node)
                exps

        merge (ExpTreeDia nodes1 edges1 (Just r1) env) (ExpTreeDia nodes2 edges2 (Just r2) _) =
          ExpTreeDia (nodes1 ++ nodes2) ((r1,r2):edges1 ++ edges2) (Just r1) env
        nextId id = foldl (\m (Node n _ _) -> 1 + (max m n)) id

graph2ast :: ExpTreeDiagram -> Maybe AST
graph2ast (ExpTreeDia _ _ Nothing _) = Nothing
graph2ast dia @ (ExpTreeDia _ _ _ env) = Just (spanningTree dia, env)
  where
    spanningTree (ExpTreeDia _ _ (Just (Node _ Varb label)) _) = Var label
    spanningTree (ExpTreeDia _ _ (Just (Node _ Numb label)) _) = Val (Num (read label))
    spanningTree (ExpTreeDia nodes edges (Just curNode @ (Node _ Appl label)) _) =
      (App label . fmap spanningTree . diaNextNodes nodes edges) curNode
    spanningTree (ExpTreeDia nodes edges (Just curNode @ (Node _ Lamb label)) _) =
      (Val . Lambda label . spanningTree . head . diaNextNodes nodes edges) curNode

    diaNextNodes nodes edges root =
      [ExpTreeDia nodes edges (Just n2) env| (n1, n2) <- edges, nodeId n1 == nodeId root]


eval :: AST -> Maybe AST
eval ast @ (Val _, _) = Just ast
eval (App name ((Val v):[]), env) =
  case lookup name env of
    Just (Lambda var exp) -> eval (exp, (var, v):env)
    _ -> Nothing
eval (App name (e1:[]), env) =
  case eval (e1, env) of
    Just (e2, _) -> eval (App name (e2:[]), env)
    _ -> Nothing
eval (Var name, env) =
  case lookup name env of
    Just val -> Just (Val val, env)
    _ -> Nothing


------------------

--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alpha_A    alpha_B
--    |          |
--    |          |
--
--    A' --f'--> B'

type A' = Expression
type B' = Maybe Expression

type A  = Maybe ExpTreeDiagram
type B  = Maybe ExpTreeDiagram

f' :: Expression -> Maybe Expression
f' = fmap unparse . (=<<) eval . parse

alpha_A :: Expression -> Maybe ExpTreeDiagram
alpha_A = fmap ast2graph . parse

f :: Maybe ExpTreeDiagram -> Maybe ExpTreeDiagram
f = fmap ast2graph . (=<<) eval . (=<<) graph2ast

alpha_B :: Maybe Expression -> Maybe ExpTreeDiagram
alpha_B = (=<<) alpha_A


-- Commutation proof:
-- alpha_B . f' == f . alpha_A

alpha_B__f' :: A' -> B
-- alpha_B__f' = alpha_B . f'
-- alpha_B__f' = (=<<) (fmap ast2graph . parse) . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph . parse) . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . fmap parse . fmap unparse . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . fmap (parse . unparse) . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . fmap return . (=<<) eval . parse
-- alpha_B__f' = join . fmap (fmap ast2graph) . return . (=<<) eval . parse
alpha_B__f' = fmap ast2graph . (=<<) eval . parse

f__alpha_A :: A' -> B
-- f__alpha_A = f . alpha_A
-- f__alpha_A = fmap ast2graph . (=<<) eval . ((=<<) graph2ast) . fmap ast2graph . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . fmap graph2ast . fmap ast2graph . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . fmap (graph2ast . ast2graph) . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . fmap return . parse
-- f__alpha_A = fmap ast2graph . (=<<) eval . join . return . parse
f__alpha_A = fmap ast2graph . (=<<) eval . parse













-- Old proof:
-- alpha . f' == f . alpha
--
-- alpha             . f'                     == f                            . alpha
-- ast2graph . parse . unparse . eval . parse == ast2graph . eval . graph2ast . ast2graph . parse
-- ast2graph .                   eval . parse == ast2graph . eval . graph2ast . ast2graph . parse
-- ast2graph .                   eval . parse == ast2graph . eval .                         parse
--                   ast2graph . eval . parse == ast2graph . eval . parse
