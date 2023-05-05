{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveTraversable #-}

module NotionalMachines.Machine.Reduct.Main where

import Control.Monad.State.Lazy

import Data.List (delete)

import Data.Maybe             (mapMaybe)
import NotionalMachines.Util.Util (maybeHead)

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
-- Reduct
--------------------
newtype ReductGame = ReductGame [ReductLevel]
data ReductLevel = ReductLevel { nodeStage  :: [ReductExp]
                                 -- ^ roots of expressions
                               , isReducing :: Bool
                               , nodeBench  :: [ReductExp]
                               , goal       :: ReductExp
                               , counter    :: Uid
                               , won        :: Bool
                               }

type Uid = Int

-- The actual JS abstraction. Type Maybe indicates holes may be empty.
data ReductExpF a = HolePlug (Maybe (ReductExpF a)) (Maybe (ReductExpF a)) a
                  | HolePipe Name (Maybe (ReductExpF a)) a
                  | Pipe Name a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
type Name = String

type ReductExp = ReductExpF Uid

rUid :: ReductExp -> Uid
rUid (HolePlug _ _ uid) = uid
rUid (HolePipe _ _ uid) = uid
rUid (Pipe _       uid) = uid

sEqual :: ReductExp -> ReductExp -> Bool
sEqual (HolePlug a1 b1 _) (HolePlug a2 b2 _) = msEqual a1 a2 && msEqual b1 b2
sEqual (HolePipe n1 a1 _) (HolePipe n2 a2 _) = n1 == n2 && msEqual a1 a2
sEqual (Pipe n1 _)        (Pipe n2 _)        = n1 == n2
sEqual _ _                                   = False

msEqual :: Maybe ReductExp -> Maybe ReductExp -> Bool
msEqual Nothing  Nothing  = True
msEqual (Just a) (Just b) = sEqual a b
msEqual _        _        = False

--------------------
-- Reduct - game play
--------------------

-- | Returns the first subtree we can find where `p` is `True`.
findSubtree :: (ReductExpF a -> Bool) -> ReductExpF a -> Maybe (ReductExpF a)
findSubtree p e = if p e then Just e else recur e
  where recur (HolePlug me1 me2 _) = mplus (findSubtree p =<< me1) (findSubtree p =<< me2)
        recur (HolePipe _   me  _) = findSubtree p =<< me
        recur (Pipe _           _) = Nothing

findNode :: Uid -> [ReductExp] -> Maybe ReductExp
findNode uid = maybeHead . mapMaybe (findSubtree ((== uid) . rUid))


-- Updates the uid in `e` and all its children to uniquely increasing values
-- starting from `n`.
updateUids :: Int -> ReductExp -> ReductExp
updateUids n e = evalState (prepareNewUids e) n

-- Prepare a State monad that sets all uids to uniquely increasing values.
prepareNewUids :: ReductExp -> State Int ReductExp
prepareNewUids = mapM (const (get <* modify succ))

-- Make new a Reduct level.
mkReductLevel :: [ReductExp] -> [ReductExp] -> ReductExp -> ReductLevel
mkReductLevel stageNodes benchNodes g = evalState go 0
  where go = do nodes1 <- mapM prepareNewUids stageNodes
                nodes2 <- mapM prepareNewUids benchNodes
                maxId  <- get
                return ReductLevel { nodeStage  = nodes1
                                   , isReducing = False
                                   , nodeBench  = nodes2
                                   , goal       = g
                                   , counter    = maxId
                                   , won        = False }

-- Check if player won.
rWon :: ReductLevel -> Bool
rWon ReductLevel { nodeStage = [e], goal = g } = sEqual e g
rWon _                                         = False

-- Apply action `a` to leval `l` and update the won field.
rApplyAction :: (ReductLevel -> ReductLevel) -> ReductLevel -> ReductLevel
rApplyAction a l = let nr = a l in nr { won = rWon nr }

-- Reduce if possible `e` in level `l`. After that, the nodes can't be changed
-- anymore (can't change program after execution starts).
aReduce :: ReductExp -> (ReductExp -> Maybe ReductExp) -> ReductLevel -> ReductLevel
aReduce e stepM l = maybe l (newLevel . updateUids (counter l)) (stepM e)
  where updateNode old new xs = new : delete old xs
        newLevel newNode = l { nodeStage  = updateNode e newNode (nodeStage l)
                             , isReducing = True
                             , counter    = rUid newNode }



-- In a reduct game level `l`, connect node `a` to hole i of node `b`.
aConnect :: ReductExp -> Int -> ReductExp -> ReductLevel -> ReductLevel
aConnect a i b l = if any (`notElem` nodeStage l) [a, b] then l
                   else l { nodeStage = tryMergeAt a b (connect i) (nodeStage l) }
  where
    connect :: Int -> ReductExp -> ReductExp -> Maybe ReductExp
    connect 0 n1 (HolePlug Nothing child uid) = Just (HolePlug (Just n1) child uid)
    connect 1 n1 (HolePlug child Nothing uid) = Just (HolePlug child (Just n1) uid)
    connect 0 n1 (HolePipe name Nothing  uid) = Just (HolePipe name (Just n1)  uid)
    connect _ _  _                            = Nothing

    -- try to merge elements x1 and x2 of xs using g, replacing them by the new
    -- element otherwise return xs.
    tryMergeAt x1 x2 g xs = let c = g x1 x2
                            in maybe xs (\new -> ((new:) . delete x1 . delete x2) xs) c


-- Bring `e` from the bench to the stage.
aBenchTake :: ReductExp -> ReductLevel -> ReductLevel
aBenchTake e l = if e `notElem` nodeBench l then l
                 else l { nodeStage = e : nodeStage l
                        , nodeBench = delete e (nodeBench l) }

-- Disconnect a node from the expression putting it back in the stage.
rDisconnect :: ReductExp -> ReductLevel -> ReductLevel
rDisconnect n l = if all (notElem (rUid n)) (nodeStage l) then l
                  else l { nodeStage = n : fmap (disconnect n) (nodeStage l) }
  where
    disconnect :: ReductExp -> ReductExp -> ReductExp
    disconnect t = mapME $ \mt -> (tryDisconnect t =<< mt) *> (disconnect t <$> mt)
      where tryDisconnect t1 t2 = if t1 == t2 then Nothing else Just t2

    -- Map over the Maybe ReductExp part
    mapME :: (Maybe ReductExp -> Maybe ReductExp) -> ReductExp -> ReductExp
    mapME g (HolePlug mt1 mt2 uid)  = HolePlug (g mt1) (g mt2) uid
    mapME g (HolePipe name mt1 uid) = HolePipe name (g mt1) uid
    mapME _ e@(Pipe {})             = e
