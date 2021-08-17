{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module NotionalMachines.Machine.AlligatorEggs.Main where

import           Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State.Lazy (State, evalState, fix, get, put)

import           NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (AsAsciiAlligators, toAscii)
import qualified NotionalMachines.Machine.AlligatorEggs.AsciiSyntax as Ascii (egg, hungryAlligator,
                                                                              oldAlligator)

import NotionalMachines.Meta.Steppable (Steppable, eval, step)


newtype Color = Color String
  deriving (Eq, Ord, Read)

instance Show Color where
  show (Color c) = c

color0 :: Color
color0 = Color "_"

toColor :: String -> Color
toColor = Color

-- TODO: i think this is incorrect. it doesn't guarantee a global fresh name.
nextColor :: Color -> Color
nextColor (Color c) = Color ('_':c)

data AlligatorFamilyF a = HungryAlligator a [AlligatorFamilyF a]
                        | OldAlligator [AlligatorFamilyF a]
                        | Egg a
  deriving (Eq, Foldable, Functor, Show, Traversable)

type AlligatorFamilies = [AlligatorFamily]
type AlligatorFamily = AlligatorFamilyF Color

instance Steppable AlligatorFamilies where
  step = eatingRule . colorRule . oldAgeRule

-- The eating rule says that if there are some families side-by-side, the
-- top-left alligator eats the family to her right.
eatingRule :: [AlligatorFamily] -> [AlligatorFamily]
eatingRule (a @ (HungryAlligator _ _):as @ (OldAlligator _:_)) = a : oldAgeRule as
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
colorRule families                                = families

recolor :: AlligatorFamily -> AlligatorFamily -> AlligatorFamily
recolor a1 a2 = evalState (mapM go a2) ([], color0)
  where -- The state contains:
        -- * A mapping of color substitutions
        -- * The next candidate color for a substitution (a counter)
        go :: Color -> State ([(Color, Color)], Color) Color
        go c | c `notElem` a1 = return c
             | otherwise = do (subs, next) <- get
                              case lookup c subs of
                                Just c2 -> return c2
                                Nothing -> let c2 = nextNotIn a1 a2 next
                                           in do put ((c, c2):subs, nextColor c2)
                                                 return c2

nextNotIn :: AlligatorFamily -> AlligatorFamily -> Color -> Color
nextNotIn xs ys = fix (\rec x -> if all (notElem x) [xs, ys] then x else rec (nextColor x))

-- When an old alligator is just guarding a single thing, it dies.
oldAgeRule :: [AlligatorFamily] -> [AlligatorFamily]
oldAgeRule (OldAlligator [protege] : rest) = protege : rest
oldAgeRule (OldAlligator proteges : rest)  = OldAlligator (step proteges) : rest
oldAgeRule families                        = families

----------------------
-- Gameplay --
----------------------
-- "The game would consist of a series of puzzles, challenging the player to
-- devise a family that, when fed X, produces Y."

-- | Check a guess made by the player.
guess :: AlligatorFamilies
      -- ^ A base family `a` with colors the user has to guess
      -- (represented by the `emptyColor = Color '?'`).
      -> [(AlligatorFamilies, AlligatorFamilies)]
      -- ^ A list of "test cases" that should be satisfied if the guess is
      -- correct.  Each test case is a pair of input and expected output. The
      -- expected output should be obtained by substituting the `emptyColor`s
      -- by the guessed `colors` in the base family `a`, putting each input in
      -- front of this family and running the game.
      -> [Color]
      -- ^ The `colors` that are being guessed (one for each `emptyColor`).
      -> Bool
guess a testCases colors = all check testCases
  where
    check (i, o) = deBruijnAlligators (eval (fillAll colors a ++ i))
                == deBruijnAlligators o
    fillAll :: [Color] -> AlligatorFamilies -> AlligatorFamilies
    fillAll cs xs = evalState (mapM (mapM fill) xs) cs
    -- Replace an emptyColor by the color in the head of the list in the state
    -- and update the state with the tails of the list.
    fill :: Color -> State [Color] Color
    fill c | c /= emptyColor = return c
           | otherwise = do cs <- get
                            case cs of
                              -- if there are no more colors to replace the emptyColor
                              -- then keep the emptyColor (nothing else we can do).
                              []   -> return c
                              -- replace empty color with next color in the list and
                              -- put the rest back in the state to be used next.
                              x:xs -> put xs >> return x

-- Represents a color to be guessed.
emptyColor :: Color
emptyColor = Color "?"

deBruijnAlligators :: [AlligatorFamilyF Color] -> [AlligatorFamilyF Int]
deBruijnAlligators = fmap (go (-1) 0 Map.empty)
  where
    go :: Int -> Int -> Map Color Int -> AlligatorFamilyF Color -> AlligatorFamilyF Int
    go freeVarIx depth env a = case a of
      HungryAlligator c as ->
        let d = succ depth
        in HungryAlligator 0 (go freeVarIx d (Map.insert c d env) <$> as)
      OldAlligator as -> OldAlligator (go freeVarIx depth env <$> as)
      Egg c -> Egg (maybe freeVarIx (depth -) (Map.lookup c env)) -- update freeVarIx

--------------------------------------------------------
-- Ascii Alligators representation of AlligatorFamily --
--------------------------------------------------------
instance Show a => AsAsciiAlligators [AlligatorFamilyF a] where
  toAscii = foldMap $ \case
    HungryAlligator c proteges -> Ascii.hungryAlligator (show c) (toAscii proteges)
    OldAlligator proteges      -> Ascii.oldAlligator (toAscii proteges)
    Egg c                      -> Ascii.egg (show c)


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
--
-- * in the "game play" example there's a problem. changing the color will result in something that doesn't have the same colors of "true" and "false" so one needs to talk about equivalence of terms with the same coloring scheme.
-- "(For this to work well, we'd need a better color rule, to explain that families with different colors in the same "pattern" are equivalent.)".




-----------------------
-- Examples of terms --
-----------------------

aid :: AlligatorFamily
aid = HungryAlligator (Color "a") [Egg (Color "a")]

-- church booleans
atru :: AlligatorFamily
atru = HungryAlligator (Color "a") [HungryAlligator (Color "b") [Egg (Color "a")]]
afls :: AlligatorFamily
afls = HungryAlligator (Color "a") [HungryAlligator (Color "b") [Egg (Color "b")]]
aand :: AlligatorFamily
aand = HungryAlligator (Color "p") [
         HungryAlligator (Color "q") [
           Egg (Color "p"), Egg (Color "q"), HungryAlligator (Color "x") [
                                                  HungryAlligator (Color "y") [
                                                    Egg (Color "y")]]]]
aor :: AlligatorFamily
aor = HungryAlligator (Color "p") [
        HungryAlligator (Color "q") [
          Egg (Color "p"), HungryAlligator (Color "x") [
                             HungryAlligator (Color "y") [
                               Egg (Color "x")]]           , Egg (Color "q")]]

-- church numbers
azero :: AlligatorFamily
azero = HungryAlligator (Color "s") [HungryAlligator (Color "z") [Egg (Color "z")]]
aone :: AlligatorFamily
aone = HungryAlligator (Color "s") [
         HungryAlligator (Color "z") [
           Egg (Color "s"), Egg (Color "z")]]
atwo :: AlligatorFamily
atwo = HungryAlligator (Color "s") [
         HungryAlligator (Color "z") [
           Egg (Color "s"), OldAlligator [
                              Egg (Color "s"), Egg (Color "z")]]]
athree :: AlligatorFamily
athree = HungryAlligator (Color "s") [
           HungryAlligator (Color "z") [
             Egg (Color "s"), OldAlligator [
                                Egg (Color "s"), OldAlligator [
                                                   Egg (Color "s"), Egg (Color "z")]]]]

-- Y-combinator
ay :: AlligatorFamily
ay = HungryAlligator (Color "g") [
       HungryAlligator (Color "x") [
         Egg (Color "g"), OldAlligator [
                            Egg (Color "x"), Egg (Color "x")]],
       HungryAlligator (Color "x") [
         Egg (Color "g"), OldAlligator [
                            Egg (Color "x"), Egg (Color "x")]]]

-- term with unknowns
aq :: AlligatorFamily
aq = HungryAlligator (Color "b") [HungryAlligator (Color "c") [(Egg (Color "?"))]]
anot :: AlligatorFamily
anot = HungryAlligator (Color "a") [Egg (Color "a"), aq, aq]
