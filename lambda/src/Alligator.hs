{-# OPTIONS_GHC -Wall -Wno-unused-top-binds -Wno-missing-pattern-synonym-signatures -Wno-unused-do-bind #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, LambdaCase #-}

module Alligator where

import Data.Function ((&))
import Data.Maybe (fromMaybe)

import Control.Monad.State.Lazy

import UntypedLambda
import Utils


newtype Color = Color Char deriving (Eq, Enum)

instance Show Color where
  show (Color c) = [c]

instance Bounded Color where
  minBound = Color 'a'
  maxBound = Color 'z'

toColor :: String -> Color
toColor [] = minBound
toColor (c:_) = Color c


data AlligatorFamilyF a = HungryAlligator a [AlligatorFamilyF a]
                        | OldAlligator [AlligatorFamilyF a]
                        | Egg a
                        deriving (Show, Eq, Functor, Foldable, Traversable)

type AlligatorFamily = AlligatorFamilyF Color

allRules :: [AlligatorFamily] -> [AlligatorFamily]
allRules = eatingRule . colorRule . oldAgeRule

evolution :: [AlligatorFamily] -> [AlligatorFamily]
evolution = fixpoint allRules

-- The eating rule says that if there are some families side-by-side, the
-- top-left alligator eats the family to her right.
eatingRule :: [AlligatorFamily] -> [AlligatorFamily]
eatingRule (a @ (HungryAlligator _ _):(as @ (OldAlligator _:_))) = a:(oldAgeRule as)
eatingRule ((HungryAlligator c p):family:rest) = fmap hatch p ++ rest
  where hatch (Egg c1) = if c == c1 then family else Egg c1
        hatch a @ (HungryAlligator c1 as) | c /= c1   = HungryAlligator c1 (fmap hatch as)
                                          | otherwise = a
        hatch (OldAlligator as) = OldAlligator (fmap hatch as)
eatingRule families = families

-- "If an alligator is about to eat a family, and there's a color that appears
-- in both families, we need to change that color in one family to something
-- else."
-- Change the colors of `family` that also appear in `a` to colors that don't
-- appear in neither family.
colorRule :: [AlligatorFamily] -> [AlligatorFamily]
colorRule (a @ (HungryAlligator _ _):family:rest) = a:recolor a family:rest
colorRule families = families

recolor :: AlligatorFamily -> AlligatorFamily -> AlligatorFamily
recolor a1 a2 = evalState (mapM go a2) ([], minBound)
  where -- The state contains:
        -- * A mapping of color substitutions
        -- * The next candidate color for a substitution (a counter)
        go :: Color -> State ([(Color, Color)], Color) Color
        go c | c `notElem` a1 = return c
             | otherwise = do (subs, next) <- get
                              case lookup c subs of
                                Just c2 -> return c2
                                Nothing -> let c2 = nextNotIn a1 a2 next
                                           in do put ((c, c2):subs, succ c2)
                                                 return c2

nextNotIn :: AlligatorFamily -> AlligatorFamily -> Color -> Color
nextNotIn xs ys = fix (\rec x -> if x `notElem` xs && x `notElem` ys then x else rec (succ x))

-- When an old alligator is just guarding a single thing, it dies.
oldAgeRule :: [AlligatorFamily] -> [AlligatorFamily]
oldAgeRule (OldAlligator (protege:[]):rest) = protege:rest
oldAgeRule (OldAlligator proteges:rest) = (OldAlligator (allRules proteges)):rest
oldAgeRule families = families

----------------------
-- Gameplay --
----------------------
-- "The game would consist of a series of puzzles, challenging the player to
-- devise a family that, when fed X, produces Y."

-- | Check a guess made by the player.
guess :: AlligatorFamily
      -- ^ A base family `a` with colors the user has to guess
      -- (represented by the `emptyColor = Color '?'`).
      -> [Color]
      -- ^ The `colors` that are being guessed (one for each `emptyColor`).
      -> [([AlligatorFamily], [AlligatorFamily])]
      -- ^ A list of "test cases" that should be satisfied if the guess is
      -- correct.  Each test case is a pair of input and expected output. The
      -- expected output should be obtained by substituting the `emptyColor`s
      -- by the guessed `colors` in the base family `a`, putting each input in
      -- front of this family and running the game.
      -> Bool
guess a colors testCases = all check testCases
  where
    check (i, o) = colorsToInts (evolution ((fillAll colors a):i))
                == colorsToInts o
    fillAll :: [Color] -> AlligatorFamily -> AlligatorFamily
    fillAll cs x = evalState (mapM fill x) cs
    fill :: Color -> State [Color] Color
    fill c | c /= emptyColor = return c
           | otherwise = do cs <- get
                            case cs of
                              [] -> return c
                              x:xs -> put xs >> return x

-- Represents a color to be guessed.
emptyColor :: Color
emptyColor = Color '?'

-------------------------
-- Lang to NM and back --
-------------------------
nm2lang :: [AlligatorFamily] -> Maybe Exp
nm2lang families =
  fmap f2e families & \case []           -> Nothing
                            me:[]        -> me
                            me1:me2:rest -> foldl (liftM2 App) (liftM2 App me1 me2) rest
  where f2e (HungryAlligator c proteges) = Lambda (show c) <$> nm2lang proteges
        f2e (OldAlligator proteges) = nm2lang proteges
        f2e (Egg c) = Just (Var (show c))

lang2nm :: Exp -> [AlligatorFamily]
lang2nm (Var name)              = [Egg (toColor name)]
lang2nm (Lambda name e)         = [HungryAlligator (toColor name) (lang2nm e)]
lang2nm (App e1 e2 @ (App _ _)) = lang2nm e1 ++ [OldAlligator (lang2nm e2)]
lang2nm (App e1 e2)             = lang2nm e1 ++ lang2nm e2

colorsToInts :: [AlligatorFamilyF Color] -> [AlligatorFamilyF Int]
colorsToInts families = fmap (go 0 []) families
  where
    go :: Int -> [(Color, Int)] -> AlligatorFamilyF Color -> AlligatorFamilyF Int
    go n s (HungryAlligator c as) = HungryAlligator n (go (succ n) ((c, n):s) <$> as)
    go n s (OldAlligator as) = OldAlligator (go n s <$> as)
    go n s (Egg c) = Egg (fromMaybe n (lookup c s))

----------------------
-- Ascii Alligators --
----------------------
type AsciiAlligators = [String]

asciiOldAlligator :: AsciiAlligators -> AsciiAlligators
asciiOldAlligator protege = alligatorBody protege : protege

asciiHungryAlligator :: String -> AsciiAlligators -> AsciiAlligators
asciiHungryAlligator var protege = (var ++ alligatorBody protege ++ "<") : (indent protege)
  where indent = map (' ':)

asciiEgg :: String -> AsciiAlligators
asciiEgg e = [e]

alligatorBody :: AsciiAlligators -> String
alligatorBody protege = replicate (width protege) '-'

width :: AsciiAlligators -> Int
width = foldl (\acc x -> max acc (length x)) 0

inFrontOf :: AsciiAlligators -> AsciiAlligators -> AsciiAlligators
inFrontOf a  [] = a
inFrontOf [] b  = b
inFrontOf a  b  = let na = padHeight a (length b)
                      nb = padHeight b (length a)
                  in glue (padWidth na) nb
  where
    padHeight x n = padWith [] n x
    padWidth  x   = map (padWith ' ' (width x)) x
    glue = zipWith (\x y -> x ++ " " ++ y)
    padWith x n xs = xs ++ replicate (n - length xs) x

--------------------------------------------------------
-- Ascii Alligators representation of AlligatorFamily --
--------------------------------------------------------
prettyAlligators :: [AlligatorFamily] -> String
prettyAlligators = unlines . go
  where go :: [AlligatorFamily] -> AsciiAlligators
        go = foldl inFrontOf [] . fmap (\case
          (HungryAlligator c proteges) -> asciiHungryAlligator (show c) (go proteges)
          (OldAlligator proteges)      -> asciiOldAlligator (go proteges)
          (Egg c)                      -> asciiEgg (show c))

--------------------------------------------
-- Ascii Alligators representation of Exp --
--------------------------------------------
exp2AlligatorAscii :: Exp -> String
exp2AlligatorAscii = unlines . go
  where
    go :: Exp -> AsciiAlligators
    go (Var name)              = asciiEgg name
    go (Lambda name e)         = asciiHungryAlligator name (go e)
    go (App e1 e2 @ (App _ _)) = (go e1) `inFrontOf` (asciiOldAlligator (go e2))
    go (App e1 e2)             = (go e1) `inFrontOf` (go e2)


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

type A  = [AlligatorFamily]
type B  = [AlligatorFamily]

f' :: A' -> B'
f' = eval

alphaA :: A' -> A
alphaA = lang2nm

f :: A -> B
f = evolution
-- f = fmap lang2nm . fmap eval . nm2lang

alphaB :: B' -> B
alphaB = alphaA


-- Commutation proof:
-- alpha_B . f' == f . alpha_A

alphaBCmpf' :: A' -> B
-- alphaBCmpf' = alphaB . f'
alphaBCmpf' = lang2nm . eval

fCmpalphaA :: A' -> B
-- fCmpalphaA = f . alphaA
fCmpalphaA = evolution . lang2nm


-- Analysis:
--
-- * Because of the overly causious alpha-conversion in the evaluation of
--   alligators, the diagram only commutes modulo variable renaming.
--
-- * Differences with lambda:
--   - Alligators (lambdas) don't have to have an egg (variable use)
--
-- * Eggs only use the colors of the alligators guarding them.
--   You can't have a blue egg without there being a blue alligator around to guard it.
--   To enforce this we need a "smart constructor"
--
-- * "But that yellow alligator sure is hungry, and there's a tasty red egg in
--   front of her. Here we go again..."
--   The original instruction describe reduction under a lambda abstraction. We
--   had to prevent that so the reduction strategy is the same as the one
--   implemented by the lambda calculus interpreter (call-by-value).
--   But _attention_, reduction under old alligations is necessary.
--
-- * Implementing the rules of the game as originally described and testing
--   them with the commutation test reveals a mistake in the substitution (eating
--   rule + color rule). The color rule is enough to avoid variable capture, but
--   eating doesn't explain what to do with bound variables. Consider the
--   following example:
--
--     a-----------< a-<
--      i-----< a-<   a
--       i b-<   a
--          a
--
--   We apply the color rule changing the color of the alligator family that is about to be eatten obtaining:
--
--     a-----------< c-<
--      i-----< a-<   c
--       i b-<   a
--          a
--
--
-- In the language of alligators, a family could be composed of multiple hungry alligators with the same color, who is guarding an egg of this color?




-----------------------
-- Examples of terms --
-----------------------

aq :: AlligatorFamily
aq = (HungryAlligator (Color 'b') [HungryAlligator (Color 'c') [(Egg (Color '?'))]])
anot :: AlligatorFamily
anot = (HungryAlligator (Color 'a') [Egg (Color 'a'),aq,aq])
atru :: AlligatorFamily
atru = (HungryAlligator (Color 'a') [HungryAlligator (Color 'b') [(Egg (Color 'a'))]])
afls :: AlligatorFamily
afls = (HungryAlligator (Color 'a') [HungryAlligator (Color 'b') [(Egg (Color 'b'))]])
