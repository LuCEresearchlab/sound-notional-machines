{-# LANGUAGE DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Alligator where

import Data.List (replicate, lines)
import Data.Maybe (fromMaybe)
import Data.Function (fix)

import UntypedLambda

-- * An Alligator can be hungry or old
-- * Eggs only use the colors of the alligators guarding them.
--   You can't have a blue egg without there being a blue alligator around to guard it.

--
-- Differences with lambda:
-- * Alligators (lambdas) don't have to have an egg (variable use)
--

newtype Color = Color Char deriving (Eq, Enum)
instance Show Color where
  show (Color c) = [c]

data AlligatorFamilyF a = HungryAlligator a [AlligatorFamilyF a]
                        | OldAlligator [AlligatorFamilyF a]
                        | Egg a
                        deriving (Show, Eq, Functor, Foldable)

type AlligatorFamily = AlligatorFamilyF Color

evolution :: [AlligatorFamily] -> [AlligatorFamily]
evolution = eatingRule . colorRule . oldAgeRule

-- The eating rule says that if there are some families side-by-side, the
-- top-left alligator eats the family to her right.
eatingRule :: [AlligatorFamily] -> [AlligatorFamily]
eatingRule (a @ (HungryAlligator _ _):family:rest) = (a `eat` family) ++ rest
eatingRule families = families

eat :: AlligatorFamily -> AlligatorFamily -> [AlligatorFamily]
eat (HungryAlligator c proteges) b = fmap hatch proteges
  where hatch (Egg c1) = if c == c1 then b else Egg c1
        hatch (HungryAlligator c1 families) = HungryAlligator c1 (fmap hatch families)
        hatch (OldAlligator families) = OldAlligator (fmap hatch families)

-- "If an alligator is about to eat a family, and there's a color that appears
-- in both families, we need to change that color in one family to something
-- else."
-- Change the colors of `family` that appear in `a`.
colorRule :: [AlligatorFamily] -> [AlligatorFamily]
colorRule (a @ (HungryAlligator _ _):family:rest) = a:(fmap (nextNotIn a) family):rest
  where nextNotIn xs = fix (\rec x -> if x `notElem` xs then x else rec (succ x))
colorRule families = families

-- When an old alligator is just guarding a single thing, it dies.
oldAgeRule :: [AlligatorFamily] -> [AlligatorFamily]
oldAgeRule (OldAlligator (protege:[]):rest) = protege:rest
oldAgeRule families = families

----------------------
-- Gameplay --
----------------------


-------------------------
-- Lang to NM and back --
-------------------------
nm2lang :: [AlligatorFamily] -> Maybe Exp
nm2lang families =
  case families of
    []                   -> Nothing
    family:[]            -> f2e family
    family1:family2:rest -> foldl (\acc x -> App <$> acc <*> f2e x) (App <$> f2e family1 <*> f2e family2) rest
  where f2e (HungryAlligator c proteges) = Lambda (show c) <$> nm2lang proteges
        f2e (OldAlligator proteges) = nm2lang proteges
        f2e (Egg c) = Just (Var (show c))

lang2nm :: Exp -> [AlligatorFamily]
lang2nm (Var (c:_))             = [Egg (Color c)]
lang2nm (Lambda (c:_) e)        = [HungryAlligator (Color c) (lang2nm e)]
lang2nm (App e1 e2 @ (App _ _)) = lang2nm e1 ++ [OldAlligator (lang2nm e2)]
lang2nm (App e1 e2)             = lang2nm e1 ++ lang2nm e2

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
    glue = zipWith (\a b -> a ++ " " ++ b)
    padWith x n xs = xs ++ replicate (n - length xs) x

--------------------------------------------------------
-- Ascii Alligators representation of AlligatorFamily --
--------------------------------------------------------
prettyAlligators :: [AlligatorFamily] -> String
prettyAlligators = unlines . alligators2ascii

alligators2ascii :: [AlligatorFamily] -> AsciiAlligators
alligators2ascii = foldl inFrontOf [] . fmap alligator2ascii

alligator2ascii :: AlligatorFamily -> AsciiAlligators
alligator2ascii (HungryAlligator c proteges) = asciiHungryAlligator (show c) (alligators2ascii proteges)
alligator2ascii (OldAlligator proteges)      = asciiOldAlligator (alligators2ascii proteges)
alligator2ascii (Egg c)                      = asciiEgg (show c)

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


-- Because of the overly causious alpha-conversion in the evaluation of
-- alligators, the diagram only commutes modulo variable renaming.
