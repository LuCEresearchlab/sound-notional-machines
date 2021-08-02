{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module NotionalMachines.Machine.ExpressionTutor (
  ExpTreeDiagram(..),
  NodeContentElem(..),
  Type,

  -- Exporting these constructor allows for the creation of diagrams that are
  -- not trees.
  Node(..),
  Edge(..),
  Plug(..),

  pattern MkNode,
  holeP,

  pattern DiaLeaf,
  pattern DiaBranch,

  newDiaLeaf,
  newDiaBranch,

  checkCycle,

  langToET,
  etToLang
  ) where

import Control.Monad.State.Lazy (State, StateT(..), get, modify, evalState, evalStateT, withStateT)
import Control.Monad (join, mplus)

import           Data.Set (Set)
import qualified Data.Set as Set

import Data.List (mapAccumL)

import NotionalMachines.Utils (maybeHead)


--------------------
-- Expression Tutor
--------------------
data ExpTreeDiagram = ExpTreeDiagram { nodes :: Set Node
                                     , edges :: Set Edge
                                     , root  :: Maybe Node }
                                     deriving (Show, Eq)
data Node = Node { nodePlug :: Plug
                 , typ      :: Maybe Type
                 -- , value    :: NodeValue
                 , content  :: [NodeContentElem] }
                 deriving (Show, Eq, Ord)
data NodeContentElem = C String
                     | NameDef String
                     | NameUse String
                     | Hole Plug -- (Maybe Type)
                     deriving (Show, Eq, Ord)
data Plug = Plug (Int, Int) deriving (Show, Eq, Ord)
data Edge = Edge Plug Plug deriving Show
type Type = String

-- the graph is undirected
instance Eq Edge where
  (Edge p1 p2) == (Edge p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
instance Ord Edge where
  compare (Edge p1 p2) (Edge p3 p4) = compare (Set.fromList [p1,p2]) (Set.fromList [p3,p4])

holes :: Node -> [Plug]
holes node = [plug | Hole plug <- content node]

-- | Hole placeholder. Use it in conjunction with MkNode so the hole will
-- contain a plug initialized to a consistent value.
holeP :: NodeContentElem
holeP = Hole (Plug (-1,-1))

-- | Smart constructor for Nodes that ensures that the Plugs are numbered
-- following this pattern:
--   - The plug on the top of each node is numbered as `Plug (n, 0)` where `n`
--   is the id of the node.
--   - The holes inside a node `j` are numbered `Plug (j, m)` where `m` goes
--   from 1 until the number of holes in the node.
pattern MkNode :: Int -> Maybe Type -> [NodeContentElem] -> Node
pattern MkNode i t parts <- (checkPlugs -> Just (Node (Plug (i,_)) t parts)) where
        MkNode i t parts = Node (Plug (i,0)) t (updateHoleIds parts)
          where updateHoleIds = snd . mapAccumL update1 1
                update1 n (Hole _) = (n+1, Hole (Plug (i,n)))
                update1 n o = (n, o) 

checkPlugs :: Node -> Maybe Node
checkPlugs n = if validPlugs n then Just n else Nothing
  where validPlugs :: Node -> Bool
        validPlugs (Node (Plug (r,s)) _ parts) =
             s == 0
          && nodeIds == replicate (length nodeIds) r
          && holeIds == [1..(length holeIds)]
            where (nodeIds, holeIds) = unzip [(i,j) | Hole (Plug (i,j)) <- parts]

--------------------
-- View the Expression Tutor graph as a tree
--------------------

-- | View a diagram as a Tree leaf. The constructor creates a diagram just with
-- a root. The pattern matches if the root has no holes (so no outgoing edges),
-- giving back that node. Notice that the existence of other nodes and edges is
-- not taken into account when matching the pattern.
pattern DiaLeaf :: Node -> ExpTreeDiagram
pattern DiaLeaf n <- ExpTreeDiagram _ _ (Just n @ (holes -> [])) where
  DiaLeaf n = ExpTreeDiagram (Set.singleton n) Set.empty (Just n)

-- | View a diagram as a Tree branch. The constructor creates a diagram rooted
-- at @r@ with outgoing edges to all the roots of @ns@.
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

-- | Returns the root node and a list of diagrams rooted at its children
diaBranch :: ExpTreeDiagram -> Maybe (Node, [ExpTreeDiagram])
diaBranch d = (\r -> (r, children r)) <$> root d
  where
    children n = [d { root = Just node } | node <- Set.elems (nodes d), node `isChild` n]
    isChild m = any (\p -> Set.member (Edge (nodePlug m) p) (edges d)) . holes

-- | Created leaf-like diagram using the constructor @c@ to create a new that
-- will be assigned a new id.
newDiaLeaf :: (Int -> Node) -> State Int ExpTreeDiagram
newDiaLeaf c = incUid (\uid -> DiaLeaf (c uid))

-- | Create a branch-like diagram using the constructor @c@ and recursively
-- continue bulding the diagram using @f@. This will connect the node created
-- with @c@ to the nodes that correspond to @xs@ and makes sure all the nodes
-- have a increasing new id.
newDiaBranch :: (Int -> Node) -- ^ constructor to create branching node.
             -> (t -> State Int ExpTreeDiagram) -- ^ function to recursively build next nodes.
             -> [t] -- ^ the terms used to construct the next nodes.
             -> State Int ExpTreeDiagram
newDiaBranch c f xs = do ys <- mapM f xs
                         incUid $ \uid -> DiaBranch (c uid) ys

incUid :: (Int -> ExpTreeDiagram) -> State Int ExpTreeDiagram
incUid g = g <$> get <* modify succ

-- | Function to call while traversing the graph to guarantee it doesn't cycle.
-- If `i` was not visited, add it to the state and perform `a` (Nothing
-- otherwise).
checkCycle :: ExpTreeDiagram -> StateT (Set Int) Maybe b -> StateT (Set Int) Maybe b
checkCycle d a = case rootId d of
                   Just i -> do visited <- get
                                if Set.member i visited then StateT (const Nothing)
                                                        else withStateT (Set.insert i) a
                   _ -> StateT (const Nothing)
  where
    rootId :: ExpTreeDiagram -> Maybe Int
    rootId = fmap ((\(Plug (i, _)) -> i) . nodePlug) . root

-- | Build a diagram from a language AST. Use `newDiaLeaf` and `newDiaBranch`.
langToET :: (t -> State Int ExpTreeDiagram) -> t -> ExpTreeDiagram
langToET f term = evalState (f term) 0

-- | Build a language AST from a diagram. Use `DiaLeaf`, `DiaBranch`, and `checkCycle`.
etToLang :: (ExpTreeDiagram -> StateT (Set Int) Maybe t) -> ExpTreeDiagram -> Maybe t
etToLang f dia = evalStateT (f dia) Set.empty




-- Commutation proof:
-- alpha_B . f' == f . alpha_A

-- alphaBCmpf' :: A' -> B
-- alphaBCmpf' = alphaB . f'
-- alphaBCmpf' = fmap alphaA . eval -- substitute alphaB and f'
-- alphaBCmpf' = fmap langToNm . evalMaybe -- substitute alphaA

-- fCmpalphaA :: A' -> B
-- fCmpalphaA = f . alphaA
-- fCmpalphaA = fmap langToNm . (=<<) eval . nmToLang . langToNm -- substitute f and alphaA
-- fCmpalphaA = fmap langToNm . (=<<) eval . return -- nmToLang and langToNm are inverses
-- fCmpalphaA = fmap langToNm . evalMaybe -- left identity on Monads


