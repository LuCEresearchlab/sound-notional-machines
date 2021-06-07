{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-pattern-synonym-signatures -Wno-unused-do-bind #-}

{-# LANGUAGE TupleSections, DeriveFunctor #-}

module Lib where

import Text.ParserCombinators.Parsec hiding (parse, State)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)

import Control.Monad.State.Lazy

import Data.Monoid

import Data.List ((\\), find, delete)

import Data.Maybe (fromMaybe, mapMaybe)

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
-- JSLC: a subset of JS corresponding to Lambda Calculus
--------------------
type Program = Exp
data Exp = App Exp Exp
         | Lambda Name Exp
         | Var Name
         deriving (Show, Read, Eq, Ord)
type Name = String

--------------------
-- Interpreter for JSLC
--------------------
step :: Program -> Program
step (App      (Lambda name e1) e2 @ (Lambda _ _)) = subst name e2 e1
step (App e1 @ (Lambda _    _ ) e2               ) = App e1 (step e2)
step (App e1                    e2               ) = App (step e1) e2
step p @ (Lambda _ _) = p
step p @ (Var _) = p

bigStep :: Program -> Program
bigStep = fixChangeThat step

eval :: Program -> Program
eval = bigStep

-- successively apply f to x until the result doesn't change
fixChangeThat :: Eq a => (a -> a) -> a -> a
fixChangeThat g x | g x == x  = x
        | otherwise = fixChangeThat g (g x)

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
-- Parsing and unparsing JSLC
--------------------
parse :: String -> Maybe Program
parse s = case Parsec.parse pProg "(unknown)" s of
            Left _ -> Nothing
            Right e -> Just e
  where pProg = pExp <* eof
        pExp = try pLambda
           <|> try pApp
           <|> pAtom
        pAtom = pVar
            <|> between (char '(') (char ')') pExp
        pVar = Var <$> pName
        pLambda = do var <- pName
                     between spaces spaces (string "=>")
                     e <- pExp
                     return $ Lambda var e
        pApp = foldl1 App <$> pAtom `sepBy1` spaces
        pName = many1 (letter <|> char '_')

unparse :: Program -> String
unparse (App e1 e2)     = parens (unwords [unparse e1, unparse e2])
unparse (Lambda name e) = parens (concat [name, " => ", unparse e])
unparse (Var name)      = name

parens :: String -> String
parens x = "(" ++ x ++ ")"

--------------------
-- Reduct
--------------------

perhaps :: (a -> Maybe a) -> a -> a
perhaps g a = fromMaybe a (g a)

-------
data ReductGame = ReductGame [ReductLevel]
data ReductLevel = ReductLevel { nodeStage  :: [ReductExp] -- roots of expressions
                               , isReducing :: Bool
                               , nodeBench  :: [ReductExp]
                               , goal       :: ReductTerm
                               , counter    :: Uid
                               , won        :: Bool }

type ReductExp = (ReductTerm, Uid)
type Uid = Int

-- The actual JS abstraction. Type Maybe indicates holes may be empty.
data ReductTerm = HolePlug (Maybe ReductExp) (Maybe ReductExp)
                | HolePipe Name (Maybe ReductExp)
                | Pipe Name
                deriving (Show, Read, Eq, Ord)


findNode :: Uid -> [ReductExp] -> Maybe ReductExp
findNode uid = find ((== uid) . snd)

-- Updates the uid in `e` and all its children to uniquely increasing values
-- starting from `n`.
updateUids :: Int -> ReductExp -> ReductExp
updateUids n e = evalState (prepareNewUids e) n

-- Prepare a State monad that sets all uids to uniquely increasing values.
prepareNewUids :: ReductExp -> State Int ReductExp
prepareNewUids = mapR setNextId
  where
    mapR :: (ReductExp -> State Int ReductExp) -> ReductExp -> State Int ReductExp
    mapR baseC e = case e of
                 (HolePlug t1 t2 , _) -> baseC =<< (, snd e) <$> (return HolePlug <*> recC t1 <*> recC t2)
                 (HolePipe name t, _) -> baseC =<< (, snd e) <$> (return (HolePipe name) <*> recC t)
                 (Pipe _         , _) -> baseC e
      where recC  = recF (mapR baseC)
            recF  = mapM
            -- merge = (++)

    setNextId :: ReductExp -> State Int ReductExp
    setNextId (t, _) = do modify succ
                          n <- get
                          return (t, n)


-- Make new a Reduct level.
mkReductLevel :: [ReductExp] -> [ReductExp] -> ReductTerm -> ReductLevel
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
rWon ReductLevel { nodeStage = [(term, _)], goal = g } = term == g
rWon _                                                 = False

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
        newLevel newNode @ (_, n) = l { nodeStage  = updateNode e newNode (nodeStage l)
                                      , isReducing = True
                                      , counter    = n }



-- In a reduct game level `l`, connect node `a` to hole i of node `b`.
aConnect :: ReductExp -> Int -> ReductExp -> ReductLevel -> ReductLevel
aConnect a i b l = if any (`notElem` (nodeStage l)) [a, b] then l
                   else l { nodeStage = tryMergeAt a b (connect i) (nodeStage l) }
  where
    connect :: Int -> ReductExp -> ReductExp -> Maybe ReductExp
    connect 0 n1 (HolePlug Nothing child, uid) = Just (HolePlug (Just n1) child, uid)
    connect 1 n1 (HolePlug child Nothing, uid) = Just (HolePlug child (Just n1), uid)
    connect 0 n1 (HolePipe name Nothing , uid) = Just (HolePipe name (Just n1) , uid)
    connect _ _  _                             = Nothing

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
rDisconnect n l = if any (notPresent n) (nodeStage l) then l
                  else l { nodeStage = n : (mapMaybe (disconnect n) (nodeStage l)) }
  where
    isPresent :: ReductExp -> ReductExp -> Bool
    isPresent a = getAny . foldMM (Any . (== a))
    notPresent a = not . (isPresent a)

    disconnect :: ReductExp -> ReductExp -> Maybe ReductExp
    disconnect a = mapMM (\b c -> if a == b then Nothing else Just c)

    mapMM :: (ReductExp -> ReductExp -> Maybe ReductExp) -> ReductExp -> Maybe ReductExp
    mapMM h e = case e of
                  (HolePlug t1 t2 , _) -> h e $ (, snd e) $ HolePlug (recC t1) (recC t2)
                  (HolePipe name t, _) -> h e $ (, snd e) $ HolePipe name (recC t)
                  (Pipe _         , _) -> h e e
      where recC = recF (mapMM h)
            recF = (=<<)

foldMM :: Monoid m => (ReductExp -> m) -> ReductExp -> m
foldMM baseC e = case e of
             (HolePlug t1 t2 , _) -> recC t1 <> recC t2 <> baseC e
             (HolePipe _ t   , _) -> recC t <> baseC e
             (Pipe _         , _) -> baseC e
  where recC = recF (foldMM baseC)
        recF = maybe mempty

--------------------
-- Lang to NM and back
--------------------
nm2lang :: ReductExp -> Maybe Program
nm2lang = reduct2jslc

reduct2jslc :: ReductExp -> Maybe Program
reduct2jslc (HolePlug n1 n2, _)  = App <$> (reduct2jslc =<< n1) <*> (reduct2jslc =<< n2)
reduct2jslc (HolePipe name n, _) = Lambda name <$> (reduct2jslc =<< n)
reduct2jslc (Pipe name, _)       = Just (Var name)

lang2nm :: Program -> ReductExp
lang2nm = jslc2reduct

jslc2reduct :: Program -> ReductExp
jslc2reduct p = (go p, 0)
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

type A' = String
type B' = Maybe String

type A  = Maybe ReductExp
type B  = Maybe ReductExp

f' :: A' -> B'
f' = fmap (unparse . eval) . parse

alphaA :: A' -> A
alphaA = fmap lang2nm . parse

f :: A -> B
f = fmap (lang2nm . eval) . (=<<) nm2lang

alphaB :: B' -> B
alphaB = (=<<) alphaA

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
