{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}

module NotionalMachines.Lang.List.Main where

data List a where
  Empty :: List a
  Cons :: a -> (List a) -> List a
  deriving (Show, Foldable)

uncons :: List a -> Maybe (a, List a)
uncons Empty = Nothing
uncons (Cons x xs) = Just (x, xs)


-- practical utility

haskellListToList :: [a] -> List a
haskellListToList = foldr Cons Empty
