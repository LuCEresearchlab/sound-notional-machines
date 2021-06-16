module Alligator where

import Data.List (replicate, lines)
import Data.Maybe (fromMaybe)

import UntypedLambda

-- * An Alligator can be hungry or old
-- * Can a family be made just of eggs?
--
-- Differences with lambda:
-- * Alligators (lambdas) don't have to have an egg (variable use)
--



--------------------------
-- Ascii representation --
--------------------------
schematicForm :: Exp -> String
schematicForm = unlines . go
  where
    go :: Exp -> [String]
    go (Var name)      = [name]
    go (Lambda name e) = hungry name (alligator (go e))
    go (App e1 e2 @ (App _ _)) = (go e1) `inFrontOf` (alligator (go e2))
    go (App e1 e2)     = (go e1) `inFrontOf` (go e2)

    alligator :: [String] -> [String]
    alligator protege = body : protege
      where body = replicate (width protege) '-'

    hungry :: String -> [String] -> [String]
    hungry var (body:protege) = (var ++ body ++ "<") : (indent protege)
      where indent = map (' ':)

    width :: [String] -> Int
    width = foldl (\acc x -> max acc (length x)) 0

    inFrontOf :: [String] -> [String] -> [String]
    inFrontOf a b = let na = padHeight a (length b)
                        nb = padHeight b (length a)
                    in glue (padWidth na) nb
      where
        padHeight x n = padWith [] n x
        padWidth  x   = map (padWith ' ' (width x)) x
        glue = zipWith (\a b -> a ++ " " ++ b)
        padWith x n xs = xs ++ replicate (n - length xs) x
