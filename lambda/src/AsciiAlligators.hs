{-# OPTIONS_GHC -Wall #-}

{-|
Description : A pretty printer using as concrete syntax an ascii version of Alligator Eggs
Stability   : experimental

A pretty printer using as concrete syntax an ascii version of Alligator Eggs (<http://worrydream.com/AlligatorEggs/>).

For example, let's implement the following instance for the untyped lambda calculus:

> instance AsAsciiAlligators Exp where
>   toAscii = \case
>     Var name           -> asciiEgg name
>     Lambda name e      -> asciiHungryAlligator name (toAscii e)
>     App e1 e2 @ App {} -> toAscii e1 `inFrontOf` asciiOldAlligator (toAscii e2)
>     App e1 e2          -> toAscii e1 `inFrontOf` toAscii e2

Now we can pretty-print the /Y-combinator/ like this:

>>> eY
Lambda "f" (App (Lambda "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lambda "x" (App (Var "f") (App (Var "x") (Var "x")))))

>>> toAscii eY
f---------------<
 x-----< x-----<
  f ---   f ---
    x x     x x


-}

module AsciiAlligators (
  AsciiAlligators,
  AsAsciiAlligators(..),

  asciiOldAlligator,
  asciiHungryAlligator,
  asciiEgg,

  inFrontOf
  ) where

-- | A representation of Alligator Eggs that can be shown as ascii art.
newtype AsciiAlligators = AsciiAl [String]

-- | A class for types that can be expressed as Alligator Eggs.
class AsAsciiAlligators a where
  toAscii :: a -> AsciiAlligators

instance Show AsciiAlligators where
  show (AsciiAl ss) = unlines ss

instance Semigroup AsciiAlligators where
  (<>) = inFrontOf

instance Monoid AsciiAlligators where
  mempty = AsciiAl []

-- | 'asciiOldAlligator' @a@ returns an old alligator protecting @a@.
asciiOldAlligator :: AsciiAlligators -> AsciiAlligators
asciiOldAlligator a @ (AsciiAl ss) = AsciiAl (alligatorBody a : ss)

-- | 'asciiHungryAlligator' @var a@ returns a hungry alligator identified by @var@ protecting @a@.
asciiHungryAlligator :: String -> AsciiAlligators -> AsciiAlligators
asciiHungryAlligator var a  @ (AsciiAl ss) = AsciiAl (hungryAlligatorBody : (indent ss))
  where indent = map (' ':)
        hungryAlligatorBody = var ++ alligatorBody a ++ "<"

-- | Returns an egg identified by a given string.
asciiEgg :: String -> AsciiAlligators
asciiEgg e = AsciiAl [e]

alligatorBody :: AsciiAlligators -> String
alligatorBody (AsciiAl protege) = replicate (width protege) '-'

width :: [String] -> Int
width = foldl max 0 . fmap length

-- | Place two ascii alligators (or eggs) one in front of another.
inFrontOf :: AsciiAlligators -> AsciiAlligators -> AsciiAlligators
inFrontOf a            (AsciiAl []) = a
inFrontOf (AsciiAl []) b            = b
inFrontOf (AsciiAl a)  (AsciiAl b)  = let na = padHeight a (length b)
                                          nb = padHeight b (length a)
                                      in AsciiAl (glue (padWidth na) nb)
  where
    padHeight x n = padWith [] n x
    padWidth  x   = map (padWith ' ' (width x)) x
    glue = zipWith (\x y -> x ++ " " ++ y)
    padWith x n xs = xs ++ replicate (n - length xs) x

