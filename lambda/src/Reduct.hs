{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-pattern-synonym-signatures -Wno-unused-do-bind #-}

{-# LANGUAGE TupleSections, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Reduct where

import Control.Monad.State.Lazy

import Data.List (delete)
import Data.Maybe (fromMaybe, mapMaybe)

import UntypedLambda
import Utils

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
data ReductGame = ReductGame [ReductLevel]
data ReductLevel = ReductLevel { nodeStage  :: [ReductExp] -- roots of expressions
                               , isReducing :: Bool
                               , nodeBench  :: [ReductExp]
                               , goal       :: ReductExp
                               , counter    :: Uid
                               , won        :: Bool }

type Uid = Int

-- The actual JS abstraction. Type Maybe indicates holes may be empty.
data ReductExpF a = HolePlug (Maybe (ReductExpF a)) (Maybe (ReductExpF a)) a
                  | HolePipe Name (Maybe (ReductExpF a)) a
                  | Pipe Name a
                  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

type ReductExp = ReductExpF Uid

rUid :: ReductExp -> Uid
rUid (HolePlug _ _ uid) = uid
rUid (HolePipe _ _ uid) = uid
rUid (Pipe _       uid) = uid

sEqual :: ReductExp -> ReductExp -> Bool
sEqual (HolePlug a1 b1 _) (HolePlug a2 b2 _) = msEqual a1 a2 && msEqual b1 b2
sEqual (HolePipe n1 a1 _) (HolePipe n2 a2 _) = n1 == n2 && msEqual a1 a2
sEqual (Pipe n1 _)        (Pipe n2 _)        = n1 == n2
sEqual _ _ = False

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
prepareNewUids = mapM (const (withState succ get))

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
aReduce :: ReductExp -> ReductLevel -> ReductLevel
aReduce e l = fromMaybe l (newLevel <$> stepReduct e)
  where stepReduct :: ReductExp -> Maybe ReductExp
        stepReduct = fmap (updateUids (counter l) . jslc2reduct . step) . reduct2jslc

        updateNode old new xs = new : (delete old xs)
        newLevel newNode = l { nodeStage  = updateNode e newNode (nodeStage l)
                             , isReducing = True
                             , counter    = rUid newNode }



-- In a reduct game level `l`, connect node `a` to hole i of node `b`.
aConnect :: ReductExp -> Int -> ReductExp -> ReductLevel -> ReductLevel
aConnect a i b l = if any (`notElem` (nodeStage l)) [a, b] then l
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
aBenchTake e l = if e `notElem` (nodeBench l) then l
                 else l { nodeStage = e : (nodeStage l)
                        , nodeBench = delete e (nodeBench l) }

-- Disconnect a node from the expression putting it back in the stage.
rDisconnect :: ReductExp -> ReductLevel -> ReductLevel
rDisconnect n l = if all (notElem (rUid n)) (nodeStage l) then l
                  else l { nodeStage = n : (fmap (disconnect n) (nodeStage l)) }
  where
    disconnect :: ReductExp -> ReductExp -> ReductExp
    disconnect t = mapME go
      where go mt = eitherOr ((tryDisconnect t) =<< mt) (disconnect t <$> mt)
            tryDisconnect t1 t2 = if t1 == t2 then Nothing else Just t2
            -- If the first is not Nothing take the second.
            eitherOr :: Maybe b -> Maybe a -> Maybe a
            eitherOr = liftM2 (flip const)

    -- Map over the Maybe ReductExp part
    mapME :: (Maybe ReductExp -> Maybe ReductExp) -> ReductExp -> ReductExp
    mapME g (HolePlug mt1 mt2 uid)  = HolePlug (g mt1) (g mt2) uid
    mapME g (HolePipe name mt1 uid) = HolePipe name (g mt1) uid
    mapME _ e @ (Pipe {}) = e

--------------------
-- Lang to NM and back
--------------------
nm2lang :: ReductExp -> Maybe Program
nm2lang = reduct2jslc

reduct2jslc :: ReductExp -> Maybe Program
reduct2jslc (HolePlug n1 n2  _) = App <$> (reduct2jslc =<< n1) <*> (reduct2jslc =<< n2)
reduct2jslc (HolePipe name n _) = Lambda name <$> (reduct2jslc =<< n)
reduct2jslc (Pipe name       _) = Just (Var name)

lang2nm :: Program -> ReductExp
lang2nm = jslc2reduct

jslc2reduct :: Program -> ReductExp
jslc2reduct p = go p 0
  where go (App e1 e2)     = HolePlug (Just (jslc2reduct e1)) (Just (jslc2reduct e2))
        go (Lambda name e) = HolePipe name (Just (jslc2reduct e))
        go (Var name)      = Pipe name

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

type A' = Exp
type B' = Exp

type A  = ReductExp
type B  = Maybe ReductExp

f' :: A' -> B'
f' = eval

alphaA :: A' -> A
alphaA = lang2nm

f :: A -> B
f = fmap (lang2nm . eval) . nm2lang

alphaB :: B' -> B
alphaB = return . alphaA

{-

[explanation of bisimulation]...

The abstraction functions (alpha) and the functions that operate on Notional Machines (f) can be either an action performed by the student or an action performed by the notional machine. Examples?


Reduct is an educational game that aims to "teach novices core programming concepts which include functions, Booleans, equality, conditionals, and mapping functions over sets".
It "uses the rules of small-step operational semantics to provide the basic units of gameplay".
The game has a sequence of level which "progressively introduces reduction rules for a subset of JavaScript ES2015".

The language effectively implemented in Reduct departures from the syntax and semantics of JavaScript in a few ways.
For example, the author describe how `x x` does not signify application, but a collection of `x`s.
So, "in Reduct, unlike lambda calculus, the expression `(x) => x x` signifies a function that outputs two copies of its input."
That could be just a difference in syntax which, although argueably misleading, wouldn't itself make the notional machine an unsound simulation of JavaScript. Let's see the impact of that an other design decisions on the soundness of the Notional Machine.

To determine the soundness of this Notional Machine, let's try to construct an abstraction function from JavaScript to Reduct (`alpha`) and determine what are the operations that transform Reduct diagrams (`f`s).
The nodes of the diagram have stages of concreteness (A1, A2, A3). In each level, the stage of concreteness doesn't change so `f :: A_n -> A_n`, and it should be possible to map a subset of JavaScript to the diagrams of any level so `alpha_n :: A' -> A_n`.
Let's first take lambda abstraction and application.

A lambda abstraction can be directly mappeded to a node in all stages.
But in the case of application, the diagram doesn't contain a node which corresponds to the application term. To apply a lambda abstraction to a term, the student must drop a term onto a lambda abstraction. The act of dropping a term onto a lambda is therefore a part of the bisulation abstraction function (alpha) because it's an act that constructs an application term from two other terms. But by dropping a term onto a lambda the application is immediately evaluated, revealing the body of the lambda after substitution. That makes the act of dropping a term onto a lambda also part of the operation of the notional machine. Dropping a term both constructs a term (application) and runs the program (reduction). The fist issue with this is that it is not possible to construct multiple application terms before triggering substitution [give example of program that can't be constructed because of that]. The second issue is that the student is then expected to continue constructing the program after part of is was already evaluated and in fact some levels can only be solved because of that.

alpha:
- nodes given to the student
- dropping nodes into lambda abstractions


## Levels with issues:

- 7:
- 9:
- 16:
- 17: can't put lambda in hole of `star == _`


## Wrong things

1. multiple returns

1. program modification

1. application by dropping

1. the Hole and Pipe metaphor if flawded because as lambdas can contains other lambdas one must identify the holes and the corresponding pipes
-}

