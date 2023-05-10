{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module NotionalMachines.Machine.AlligatorEggs.Main (

      AlligatorFamilyF (..)
    , AlligatorFamily
    , evolve
    , recolor
    , deBruijnAlligators

    , guess

    -- Exmples
    , aid
    -- church booleans
    , atru
    , afls
    , aand
    , aor
    -- church numbers
    , azero
    , aone
    , atwo
    , athree
    -- Y-combinator
    , ay
    -- term with unknowns
    , aq
    , anot
    ) where

import Control.Monad.State.Lazy (State, evalState, get, put)

import           Data.Map (Map)
import qualified Data.Map as Map

import Data.Colour.Names (black)

import Prettyprinter (Pretty, pretty, vsep)

import NotionalMachines.Meta.Steppable (Steppable (step), SteppableM, evalM, stepM)

import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color (..))

import           NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (AsAsciiAlligators, toAscii)
import qualified NotionalMachines.Machine.AlligatorEggs.AsciiSyntax as Ascii (egg, hungryAlligator,
                                                                              oldAlligator)


--------------------------
-- Alligator Eggs
--------------------------

data AlligatorFamilyF a = HungryAlligator a [AlligatorFamilyF a]
                        | OldAlligator [AlligatorFamilyF a]
                        | Egg a
  deriving (Eq, Foldable, Functor, Show, Traversable)

type AlligatorFamilies = [AlligatorFamily]
type AlligatorFamily = AlligatorFamilyF Color


instance (Eq a, Enum a) => Steppable [AlligatorFamilyF a] where
  step = evolve

instance (Eq a, Enum a) => SteppableM [AlligatorFamilyF a] Maybe where
  stepM = fmap evolve . checkThat eggColoredCorrectly

-- | Taking a step consists of using the first rule that applies (order matters).
-- TODO: talk about the fact that evolve doesn't correspond to step from lambda
-- (e.g. oldAlligator rule, :asciiTrace (\a.a) ((\b.b) c)). Talk about non-lock-step simulation.
evolve :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
evolve = applyRules [oldAgeRule, colorRule, eatingRule]

-- | Given a list of functions, apply each one in sequence until one of them
-- returns a different value.
applyRules :: Eq a => [a -> a] -> a -> a
applyRules [] a     = a
applyRules (f:fs) a = if f a == a then applyRules fs a else f a

-- The eating rule says that if there are some families side-by-side, the
-- top-left alligator eats the family to her right.
eatingRule :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
eatingRule ((HungryAlligator c proteges):family:rest) = map hatch proteges ++ rest
  where hatch (Egg c1)                | c == c1 = family
        -- hatch (HungryAlligator c1 ys)           = HungryAlligator c1 (map hatch ys)
        hatch (HungryAlligator c1 ys) | c /= c1 = HungryAlligator c1 (map hatch ys) -- NM fix
        hatch (OldAlligator ys)                 = OldAlligator (map hatch ys)
        hatch protege                           = protege
eatingRule families = families

-- "If an alligator is about to eat a family, and there's a color that appears
-- in both families, we need to change that color in one family to something
-- else."
-- Change the colors of `family` that also appear in `a` to colors that don't
-- appear in neither family.
colorRule :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
colorRule (a@(HungryAlligator _ _):family:rest) = a:recolor a family:rest
colorRule families                              = families

recolor :: forall a. (Enum a, Eq a) => AlligatorFamilyF a -> AlligatorFamilyF a -> AlligatorFamilyF a
recolor a1 a2 = evalState (mapM go a2) ([], toEnum 0)
  where -- The state contains:
        -- * A mapping of color substitutions
        -- * The next candidate color for a substitution (a counter)
        go :: a -> State ([(a, a)], a) a
        go c | c `notElem` a1 = return c
             | otherwise = do (subs, next) <- get
                              case lookup c subs of
                                Just newC -> return newC
                                Nothing -> let newC = nextNotIn [a1, a2] next
                                           in do put ((c, newC):subs, succ newC)
                                                 return newC
          where
            nextNotIn :: (Enum a, Eq a) => [AlligatorFamilyF a] -> a -> a
            nextNotIn as = until (\x -> all (notElem x) as) succ

-- When an old alligator is just guarding a single thing, it dies.
oldAgeRule :: (Eq a, Enum a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
oldAgeRule (OldAlligator        [] : rest) = rest
oldAgeRule (OldAlligator [protege] : rest) = protege : rest
oldAgeRule (OldAlligator proteges  : rest) = OldAlligator (evolve proteges) : rest -- needed in CBV
oldAgeRule (a : rest@(OldAlligator _:_))   = a : evolve rest -- needed in CBV
oldAgeRule families                        = families

-- Check that all eggs are guarded by a hungry alligator with the same color.
eggColoredCorrectly :: forall a. (Eq a) => [AlligatorFamilyF a] -> Bool
eggColoredCorrectly = go []
    where go :: [a] -> [AlligatorFamilyF a] -> Bool
          go cs = all $ \case HungryAlligator c as -> go (c:cs) as
                              OldAlligator as      -> go cs as
                              Egg c                -> c `elem` cs

checkThat :: (p -> Bool) -> p -> Maybe p
checkThat f x = if f x then Just x else Nothing

----------------------
-- Gameplay --
----------------------
-- "The game would consist of a series of puzzles, challenging the player to
-- devise a family that, when fed X, produces Y."

-- | Represents a color to be guessed. This color is not generatable with the Enum instance.
jokerColor :: Color
jokerColor = MkColor black

-- | Check a guess made by the player.
guess :: AlligatorFamilies
      -- ^ A base family `a` with colors the user has to guess
      -- (represented by the `jokerColor`).
      -> [(AlligatorFamilies, AlligatorFamilies)]
      -- ^ A list of "test cases" that should be satisfied if the guess is
      -- correct.  Each test case is a pair of input and expected output. The
      -- expected output should be obtained by substituting the `jokerColor`s
      -- by the guessed `colors` in the base family `a`, putting each input in
      -- front of this family and running the game.
      -> [Color]
      -- ^ The `colors` that are being guessed (one for each `jokerColor`).
      -> Bool
guess a testCases colors = all check testCases
  where
    check (i, expected) = case evalM (fillAll colors a ++ i) of
                            Nothing -> False
                            Just toCheck -> eggColoredCorrectly toCheck
                                         && deBruijnAlligators toCheck == deBruijnAlligators expected
    fillAll :: [Color] -> AlligatorFamilies -> AlligatorFamilies
    fillAll cs xs = evalState (mapM (mapM fill) xs) cs
    -- Replace a jokerColor by the color in the head of the list in the state
    -- and update the state with the tail of the list.
    fill :: Color -> State [Color] Color
    fill c | c /= jokerColor = return c
           | otherwise = get >>= \case
                                    -- if there are no more colors to replace the jokerColor
                                    -- then keep the jokerColor (nothing else we can do).
                                    []   -> return c
                                    -- replace empty color with next color in the list and
                                    -- put the rest back in the state to be used next.
                                    x:xs -> put xs >> return x

deBruijnAlligators :: [AlligatorFamilyF Color] -> [AlligatorFamilyF Int]
deBruijnAlligators = fmap (go (-1) 0 Map.empty)
  where
    go :: Int -> Int -> Map Color Int -> AlligatorFamilyF Color -> AlligatorFamilyF Int
    go freeVarIx depth env = \case
      HungryAlligator c as -> let d = succ depth
                              in HungryAlligator 0 (go freeVarIx d (Map.insert c d env) <$> as)
      OldAlligator as -> OldAlligator (go freeVarIx depth env <$> as)
      Egg c -> Egg (maybe freeVarIx (depth -) (Map.lookup c env)) -- update freeVarIx

--------------------------------------------------------
-- Ascii Alligators representation of AlligatorFamily --
--------------------------------------------------------
instance AsAsciiAlligators AlligatorFamilies where
  toAscii = foldMap $ \case
    HungryAlligator (MkColorFromName n) proteges -> Ascii.hungryAlligator n (toAscii proteges)
    OldAlligator proteges                        -> Ascii.oldAlligator (toAscii proteges)
    Egg (MkColorFromName n)                      -> Ascii.egg n

instance {-# OVERLAPPING #-} Pretty AlligatorFamilies where
  pretty = pretty . toAscii

instance {-# OVERLAPPING #-} Pretty [AlligatorFamilies] where
  pretty = vsep . map pretty

-- Analysis:
--
-- * Because of the overly causious alpha-conversion in the evaluation of
--   alligators, the diagram only commutes modulo variable renaming.
--
-- * Differences with lambda:
--   - Alligators (lambdas) don't have to have an egg (variable use)
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
--
-- * "Notice that eggs only use the colors of the alligators guarding them. You can't have a blue egg without there being a blue alligator around to guard it."
--   At first glance, this rule makes sence. It means that in a complete program there can be no free variables. But this means that many text book examples, such as the one in TAPL page 59 can be represented but are not considered valid because would lead to eggs that are not covered.



--  Alligators
--    commutation proof:                                                                                                           FAIL (0.04s)
--        ✗ commutation proof failed at test/Spec.hs:139:3
--          after 8 tests and 9 shrinks.

--              ┏━━ test/Spec.hs ━━━
--          136 ┃ isEquivalentTo :: (Eq a, Show a, Show e) => Gen e -> (e -> a) -> (e -> a) -> Property
--          137 ┃ isEquivalentTo g f f' = prop $ do
--          138 ┃   e <- forAll g
--              ┃   │ App
--              ┃   │   (Lambda
--              ┃   │      "a"
--              ┃   │      (App
--              ┃   │         (Lambda "g" (App (Var "g") (Lambda "b" (Var "a"))))
--              ┃   │         (Lambda "a" (Var "a"))))
--              ┃   │   (Lambda "a" (Var "a"))
--          139 ┃   f e === f' e
--              ┃   ^^^^^^^^^^^^
--              ┃   │ ━━━ Failed (- lhs) (+ rhs) ━━━
--              ┃   │ - Just [ HungryAlligator 0 [ HungryAlligator 0 [ Egg 0 ] ] ]
--              ┃   │ + Just [ HungryAlligator 0 [ Egg 0 ] ]

--          This failure can be reproduced by running:
--          > recheck (Size 7) (Seed 11253128347395396264 14482578956410378323) commutation proof

--      Use '--hedgehog-replay "Size 7 Seed 11253128347395396264 14482578956410378323"' to reproduce.



-----------------------
-- Examples of terms --
-----------------------

aid :: AlligatorFamily
aid = HungryAlligator (MkColorFromName "a") [Egg (MkColorFromName "a")]

-- church booleans
atru :: AlligatorFamily
atru = HungryAlligator (MkColorFromName "a") [HungryAlligator (MkColorFromName "b") [Egg (MkColorFromName "a")]]
afls :: AlligatorFamily
afls = HungryAlligator (MkColorFromName "a") [HungryAlligator (MkColorFromName "b") [Egg (MkColorFromName "b")]]
aand :: AlligatorFamily
aand = HungryAlligator (MkColorFromName "p") [
         HungryAlligator (MkColorFromName "q") [
           Egg (MkColorFromName "p"), Egg (MkColorFromName "q"), HungryAlligator (MkColorFromName "x") [
                                                  HungryAlligator (MkColorFromName "y") [
                                                    Egg (MkColorFromName "y")]]]]
aor :: AlligatorFamily
aor = HungryAlligator (MkColorFromName "p") [
        HungryAlligator (MkColorFromName "q") [
          Egg (MkColorFromName "p"), HungryAlligator (MkColorFromName "x") [
                             HungryAlligator (MkColorFromName "y") [
                               Egg (MkColorFromName "x")]]           , Egg (MkColorFromName "q")]]

-- church numbers
azero :: AlligatorFamily
azero = HungryAlligator (MkColorFromName "s") [HungryAlligator (MkColorFromName "z") [Egg (MkColorFromName "z")]]
aone :: AlligatorFamily
aone = HungryAlligator (MkColorFromName "s") [
         HungryAlligator (MkColorFromName "z") [
           Egg (MkColorFromName "s"), Egg (MkColorFromName "z")]]
atwo :: AlligatorFamily
atwo = HungryAlligator (MkColorFromName "s") [
         HungryAlligator (MkColorFromName "z") [
           Egg (MkColorFromName "s"), OldAlligator [
                              Egg (MkColorFromName "s"), Egg (MkColorFromName "z")]]]
athree :: AlligatorFamily
athree = HungryAlligator (MkColorFromName "s") [
           HungryAlligator (MkColorFromName "z") [
             Egg (MkColorFromName "s"), OldAlligator [
                                Egg (MkColorFromName "s"), OldAlligator [
                                                   Egg (MkColorFromName "s"), Egg (MkColorFromName "z")]]]]

-- Y-combinator
ay :: AlligatorFamily
ay = HungryAlligator (MkColorFromName "g") [
       HungryAlligator (MkColorFromName "x") [
         Egg (MkColorFromName "g"), OldAlligator [
                            Egg (MkColorFromName "x"), Egg (MkColorFromName "x")]],
       HungryAlligator (MkColorFromName "x") [
         Egg (MkColorFromName "g"), OldAlligator [
                            Egg (MkColorFromName "x"), Egg (MkColorFromName "x")]]]

-- term with unknowns
aq :: AlligatorFamily
aq = HungryAlligator (MkColorFromName "b") [HungryAlligator (MkColorFromName "c") [Egg jokerColor]]
anot :: AlligatorFamily
anot = HungryAlligator (MkColorFromName "a") [Egg (MkColorFromName "a"), aq, aq]



--     Alligators
--       commutation proof:                                                                                                           FAIL (0.12s)
--           ✗ commutation proof failed at test/Spec.hs:137:7
--             after 193 tests and 15 shrinks.
--
--                 ┏━━ test/Spec.hs ━━━
--             134 ┃ isEquivalentTo :: (Eq a, Show a, Show e) => Gen e -> (e -> a) -> (e -> a) -> Property
--             135 ┃ isEquivalentTo g f f' = prop $ do
--             136 ┃   e <- forAll g
--                 ┃   │ￔ� App
--                 ┃   │ￔ�   (Lambda
--                 ┃   │ￔ�      "a"
--                 ┃   │ￔ�      (App
--                 ┃   │ￔ�         (Lambda
--                 ┃   │ￔ�            "a"
--                 ┃   │ￔ�            (App
--                 ┃   │ￔ�               (Lambda "b" (Var "a")) (Lambda "a" (App (Var "a") (Var "a")))))
--                 ┃   │ￔ�         (Lambda "a" (Var "a"))))
--                 ┃   │ￔ�   (Lambda "a" (Var "a"))
--             137 ┃   f e === f' e
--                 ┃   ^^^^^^^^^^^^
--                 ┃   │ￔ� ━━━ Exception (ErrorCall) ━━━
--                 ┃   │ￔ� Prelude.!!: negative index
--
--             This failure can be reproduced by running:
--             > recheck (Size 92) (Seed 9259115493789069110 4342968287765638441) commutation proof
--
--         Use '--hedgehog-replay "Size 92 Seed 9259115493789069110 4342968287765638441"' to reproduce.
--
--         Use -p '/Alligators.commutation proof/' to rerun this test only.
