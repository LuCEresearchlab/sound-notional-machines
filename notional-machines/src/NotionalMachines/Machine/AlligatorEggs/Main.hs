{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module NotionalMachines.Machine.AlligatorEggs.Main where

import qualified Data.Colour as C (Colour)
import           Data.Colour.RGBSpace.HSV (RGB, hsv, hue)
import           Data.Colour.SRGB (sRGB24show)
import           Data.Colour.Names (black)
import           Data.Colour.Palette.ColorSet (infiniteWebColors)

import Control.Monad.State.Lazy (State, evalState, get, put)

import Data.Maybe (fromJust)

import           Data.List (unfoldr, elemIndex)
import           Data.Map  (Map)
import qualified Data.Map  as Map

import           NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (AsAsciiAlligators, toAscii)
import qualified NotionalMachines.Machine.AlligatorEggs.AsciiSyntax as Ascii (egg, hungryAlligator,
                                                                              oldAlligator)
import           NotionalMachines.Meta.Steppable                    (SteppableM, evalM, stepM)


------------------------------------------------------------------
-- Map from variable name to colour (and back) via a mapping from name to Int (and back)
------------------------------------------------------------------
-- Fundamentaly, the space of names is infinite, and the space of Colour Double
-- is not because Double is finite. Nevertheless, the conceptual point of the
-- mapping from Untyped Lambda Calculus and Alligator Eggs still stands (module
-- that limitation) so we want to find a mapping of names to colours that is
-- useful.
--
-- ps.: This would be easy with automatic backward function generation =====

--------------------------
-- Map from name to Int
--------------------------
nameToInt :: String -> Int
nameToInt = changeBase letterBase . map charToInt
intToName :: Int -> String
intToName = map intToChar . changeBaseInv letterBase

charToInt :: Char -> Int
charToInt = (\i -> i - fromEnum 'a') . fromEnum
intToChar :: Int -> Char
intToChar = toEnum . (\i -> i + fromEnum 'a')

changeBase :: Int -> [Int] -> Int
changeBase base = sum . zipWith (\i n -> n * base^i) [0..]
changeBaseInv :: Int -> Int -> [Int]
changeBaseInv base = (\l -> if null l then [0] else l) . f
    where f = unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` base, b `div` base))

letterBase :: Int
letterBase = 26 -- letters are numbers in base 26


--------------------------
-- Map from Int to Color (v1)
--------------------------

-- colorToNat :: C.Colour Double -> Int
-- colorToNat = hueToInt defaultSpacing . rgbToHue . toSRGB
-- natToColor :: Int -> C.Colour Double
-- natToColor = uncurryRGB sRGB . hueToRGB . intToHue defaultSpacing

hueToRGB :: Double -> RGB Double
hueToRGB h = hsv h 1 1
rgbToHue :: RGB Double -> Double
rgbToHue = hue

nameToHue :: String -> Double
nameToHue = intToHue defaultSpacing . nameToInt
hueToName :: Double -> String
hueToName = intToName . hueToInt defaultSpacing

intToHue :: Int -> Int -> Double
intToHue spc x = fromIntegral (x `mod` spc) / fromIntegral spc * 360 + additive
  where additive = fromIntegral (x `div` spc)
  -- where additive = (360/fromIntegral spc) / fromIntegral (2^(x `div` spc))
hueToInt :: Int -> Double -> Int
hueToInt spc x = h x + defaultSpacing * (round x - round (360 / fromIntegral spc) * h x)
  where h y = round (fromIntegral defaultSpacing * (y / 360))

defaultSpacing :: Int
defaultSpacing = 6 -- space between one colour and the next

--------------------------
-- Another approach to map from Int to Color (v2)
--------------------------
-- The focus here is to generate colors that are convenient for visualization,
-- with good contrast and generally subjectvely pleasant to look at.

colorToNat :: C.Colour Double -> Int
colorToNat c = fromJust $ elemIndex c convenientColors
natToColor :: Int -> C.Colour Double
natToColor i = convenientColors !! i

takePeriod :: Int -> [b] -> [b]
takePeriod p l = map (\i -> l !! (i*p)) [0..]

-- This list is infinie so indexing into it is guaranteed
convenientColors :: [C.Colour Double]
convenientColors = takePeriod 13 $ tail infiniteWebColors
------------------------------------------------------------------
------------------------------------------------------------------

--------------------------
-- Colors
--------------------------
-- Wrapping around colors convenient to map to/from names and with Enum and Ord
-- instances that make color substitution and recoloring simpler.

newtype Color = MkColor { colorContent :: C.Colour Double }
  deriving (Eq, Read, Show)

instance Enum Color where
  toEnum = MkColor . natToColor
  fromEnum = colorToNat . colorContent

instance Ord Color where
  compare (MkColor c1) (MkColor c2) = compare (colorToNat c1) (colorToNat c2)

nameToColor :: String -> Color
nameToColor = MkColor . natToColor . nameToInt

colorToName :: Color -> String
colorToName = intToName . colorToNat . colorContent

colorHexa :: Color -> String
colorHexa = sRGB24show . colorContent
--------------------------

--------------------------
-- Alligator Eggs
--------------------------

data AlligatorFamilyF a = HungryAlligator a [AlligatorFamilyF a]
                        | OldAlligator [AlligatorFamilyF a]
                        | Egg a
  deriving (Eq, Foldable, Functor, Show, Traversable)

type AlligatorFamilies = [AlligatorFamily]
type AlligatorFamily = AlligatorFamilyF Color


instance (Eq a, Enum a) => SteppableM [AlligatorFamilyF a] Maybe where
  stepM = fmap evolve . checkThat eggColoredCorrectly

-- | Taking a step consists of applying each rule in sequence: oldAgeRule, colorRule, eatingRule.
-- The values are Maybes because a family may be malformed if it is not colored properly.
evolve :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
evolve = eatingRule . colorRule . oldAgeRule

-- The eating rule says that if there are some families side-by-side, the
-- top-left alligator eats the family to her right.
eatingRule :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
eatingRule (a@(HungryAlligator _ _) : as@(OldAlligator _:_)) = a : oldAgeRule as
eatingRule ((HungryAlligator c p):family:rest) = map hatch p ++ rest
  -- where hatch (HungryAlligator c1 as) = HungryAlligator c1 (map hatch as)
  where hatch a@(HungryAlligator c1 as) | c /= c1   = HungryAlligator c1 (map hatch as)
                                        | otherwise = a
        hatch (Egg c1) | c == c1   = family
                       | otherwise = Egg c1
        hatch (OldAlligator as) = OldAlligator (map hatch as)
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
oldAgeRule (OldAlligator proteges  : rest) = OldAlligator (evolve proteges) : rest
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
instance AsAsciiAlligators [AlligatorFamilyF Color] where
  toAscii = foldMap $ \case
    HungryAlligator c proteges -> Ascii.hungryAlligator (colorToName c) (toAscii proteges)
    OldAlligator proteges      -> Ascii.oldAlligator (toAscii proteges)
    Egg c                      -> Ascii.egg (colorToName c)


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
aid = HungryAlligator (nameToColor "a") [Egg (nameToColor "a")]

-- church booleans
atru :: AlligatorFamily
atru = HungryAlligator (nameToColor "a") [HungryAlligator (nameToColor "b") [Egg (nameToColor "a")]]
afls :: AlligatorFamily
afls = HungryAlligator (nameToColor "a") [HungryAlligator (nameToColor "b") [Egg (nameToColor "b")]]
aand :: AlligatorFamily
aand = HungryAlligator (nameToColor "p") [
         HungryAlligator (nameToColor "q") [
           Egg (nameToColor "p"), Egg (nameToColor "q"), HungryAlligator (nameToColor "x") [
                                                  HungryAlligator (nameToColor "y") [
                                                    Egg (nameToColor "y")]]]]
aor :: AlligatorFamily
aor = HungryAlligator (nameToColor "p") [
        HungryAlligator (nameToColor "q") [
          Egg (nameToColor "p"), HungryAlligator (nameToColor "x") [
                             HungryAlligator (nameToColor "y") [
                               Egg (nameToColor "x")]]           , Egg (nameToColor "q")]]

-- church numbers
azero :: AlligatorFamily
azero = HungryAlligator (nameToColor "s") [HungryAlligator (nameToColor "z") [Egg (nameToColor "z")]]
aone :: AlligatorFamily
aone = HungryAlligator (nameToColor "s") [
         HungryAlligator (nameToColor "z") [
           Egg (nameToColor "s"), Egg (nameToColor "z")]]
atwo :: AlligatorFamily
atwo = HungryAlligator (nameToColor "s") [
         HungryAlligator (nameToColor "z") [
           Egg (nameToColor "s"), OldAlligator [
                              Egg (nameToColor "s"), Egg (nameToColor "z")]]]
athree :: AlligatorFamily
athree = HungryAlligator (nameToColor "s") [
           HungryAlligator (nameToColor "z") [
             Egg (nameToColor "s"), OldAlligator [
                                Egg (nameToColor "s"), OldAlligator [
                                                   Egg (nameToColor "s"), Egg (nameToColor "z")]]]]

-- Y-combinator
ay :: AlligatorFamily
ay = HungryAlligator (nameToColor "g") [
       HungryAlligator (nameToColor "x") [
         Egg (nameToColor "g"), OldAlligator [
                            Egg (nameToColor "x"), Egg (nameToColor "x")]],
       HungryAlligator (nameToColor "x") [
         Egg (nameToColor "g"), OldAlligator [
                            Egg (nameToColor "x"), Egg (nameToColor "x")]]]

-- term with unknowns
aq :: AlligatorFamily
aq = HungryAlligator (nameToColor "b") [HungryAlligator (nameToColor "c") [Egg jokerColor]]
anot :: AlligatorFamily
anot = HungryAlligator (nameToColor "a") [Egg (nameToColor "a"), aq, aq]
