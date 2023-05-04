{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Machine.ExpressionTree.Main where

data ExpAsTree = Box String
               | BinaryBox ExpAsTree ExpAsTree
               | LambdaBox String ExpAsTree
  deriving (Eq, Show)

-- | Convert a lambda expression to a tree in ASCII format with branches and leaves.
toAscii :: ExpAsTree -> String
toAscii = unlines . go
  where
    go :: ExpAsTree -> [String]
    go (Box s)           = [s]
    go (BinaryBox e1 e2) =     "App" :
                           pad "├── " "│   " (go e1) ++
                           pad "└── " "    " (go e2)
    go (LambdaBox s e)   =    ("Lambda " ++ s) :
                           pad "└── " "    " (go e)

    pad :: String -> String -> [String] -> [String]
    pad _ _ []            = []
    pad first rest (x:xs) = (first ++ x) : map (rest ++) xs

