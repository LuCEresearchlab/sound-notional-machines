{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NotionalMachines.Machine.AlligatorEggs.Main where

import Data.Colour.RGBSpace.HSV (RGB, hsv, hue)

import           Control.Monad.State.Lazy (State, evalState, get, put)
import           Data.List                (unfoldr)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import           NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (AsAsciiAlligators, toAscii)
import qualified NotionalMachines.Machine.AlligatorEggs.AsciiSyntax as Ascii (egg, hungryAlligator,
                                                                              oldAlligator)
import           NotionalMachines.Meta.Steppable                    (Steppable, eval, step)



-- Fundamentaly, the space of names is infinite, and the space of Colour Double
-- is not because Double is finite. Nevertheless, the conceptual point of the
-- mapping from Untyped Lambda Calculus and Alligator Eggs still stands (module
-- that limitation) so we want to find a mapping of names to colours that is
-- useful.
--
-- ===== Invertible functions: This would be easy with automatic backward function generation =====
nameToRGB :: String -> RGB Double
nameToRGB = hueToRGB . nameToHue
rgbToName :: RGB Double -> String
rgbToName = hueToName . rgbToHue

hueToRGB :: Double -> RGB Double
hueToRGB h = hsv h 1 1
rgbToHue :: RGB Double -> Double
rgbToHue = hue

nameToHue :: String -> Double
nameToHue = intToHue defaultSpacing . nameToInt
hueToName :: Double -> String
hueToName = intToName . hueToInt defaultSpacing

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

intToHue :: Int -> Int -> Double
intToHue spc x = fromIntegral (x `mod` spc) / fromIntegral spc * 360 + fromIntegral (x `div` spc)
hueToInt :: Int -> Double -> Int
hueToInt spc x = h x + defaultSpacing * (round x - round (360 / fromIntegral spc) * h x)
  where h y = round (fromIntegral defaultSpacing * (y / 360))
-- ================================================================================================

defaultSpacing :: Int
defaultSpacing = 6 -- space between one colour and the next

letterBase :: Int
letterBase = 26 -- letters are numbers in base 26


newtype Color = MkColor { colorContent :: RGB Double }
  deriving (Eq, Read, Show)

instance Enum Color where
  toEnum = MkColor . hueToRGB . fromIntegral
  fromEnum = round . rgbToHue . colorContent

instance Ord Color where
  compare (MkColor c1) (MkColor c2) = compare (rgbToHue c1) (rgbToHue c2)

nameToColor :: String -> Color
nameToColor = MkColor . nameToRGB

colorToName :: Color -> String
colorToName = rgbToName . colorContent


data AlligatorFamilyF a = HungryAlligator a [AlligatorFamilyF a]
                        | OldAlligator [AlligatorFamilyF a]
                        | Egg a
  deriving (Eq, Foldable, Functor, Show, Traversable)

type AlligatorFamilies = [AlligatorFamily]
type AlligatorFamily = AlligatorFamilyF Color


-- TODO:
-- * Eggs only use the colors of the alligators guarding them.
--   You can't have a blue egg without there being a blue alligator around to guard it.
--   To enforce this we need a "smart constructor" or change the types?


instance (Eq a, Enum a) => Steppable [AlligatorFamilyF a] where
  step = eatingRule . colorRule . oldAgeRule

-- The eating rule says that if there are some families side-by-side, the
-- top-left alligator eats the family to her right.
eatingRule :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
eatingRule (a @ (HungryAlligator _ _):as @ (OldAlligator _:_)) = a : oldAgeRule as
eatingRule ((HungryAlligator c p):family:rest) = fmap hatch p ++ rest
  where hatch a @ (HungryAlligator c1 as) | c /= c1   = HungryAlligator c1 (fmap hatch as)
                                          | otherwise = a
        hatch (Egg c1) | c == c1   = family
                       | otherwise = Egg c1
        hatch (OldAlligator as) = OldAlligator (fmap hatch as)
eatingRule families = families

-- "If an alligator is about to eat a family, and there's a color that appears
-- in both families, we need to change that color in one family to something
-- else."
-- Change the colors of `family` that also appear in `a` to colors that don't
-- appear in neither family.
colorRule :: (Enum a, Eq a) => [AlligatorFamilyF a] -> [AlligatorFamilyF a]
colorRule (a @ (HungryAlligator _ _):family:rest) = a:recolor a family:rest
colorRule families                                = families

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
oldAgeRule (OldAlligator proteges  : rest) = OldAlligator (step proteges) : rest
oldAgeRule families                        = families

----------------------
-- Gameplay --
----------------------
-- "The game would consist of a series of puzzles, challenging the player to
-- devise a family that, when fed X, produces Y."

-- | Represents a color to be guessed. This color is not generatable with the Enum instance.
jokerColor :: Color
jokerColor = MkColor (hsv 0 0 0)

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
    check (i, o) = deBruijnAlligators (eval (fillAll colors a ++ i))
                == deBruijnAlligators o
    fillAll :: [Color] -> AlligatorFamilies -> AlligatorFamilies
    fillAll cs xs = evalState (mapM (mapM fill) xs) cs
    -- Replace a jokerColor by the color in the head of the list in the state
    -- and update the state with the tail of the list.
    fill :: Color -> State [Color] Color
    fill c | c /= jokerColor = return c
           | otherwise = do cs <- get
                            case cs of
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
    go freeVarIx depth env a = case a of
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
