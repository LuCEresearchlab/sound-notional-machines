{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}

module NotionalMachines.Machine.ExpressionTree.Diagram where

import Data.Maybe (fromMaybe)
import Data.Tree  (Tree (Node))

import Diagrams.Backend.Rasterific      (B)
import Diagrams.Backend.Rasterific.Text (texterific)
import Diagrams.Prelude                 (Angle, Diagram, IsName, applyAll, centerXY, circle,
                                         connectPerim, extentX, extentY, fc, hcat, height, lwO,
                                         named, pad, rad, rect, roundedRect, scale, tau, text,
                                         white, width, with, (#), (&), (.~), (@@), (~~))
import Diagrams.TwoD.Layout.Tree        (renderTree, renderTree', slHSep, slHeight, slVSep, slWidth,
                                         symmLayout')

import Control.Monad.State                          (State, gets, modify, runState)
import Data.Bifunctor                               (bimap, first, second)
import NotionalMachines.Machine.ExpressionTree.Main (ExpAsTree (..))


toDiagram2 :: Double -> ExpAsTree -> Diagram B
toDiagram2 size = renderT . go
  where
    go :: ExpAsTree -> Tree (Diagram B)
    go = \case
      Box n           -> Node (framed n) []
      BinaryBox e1 e2 -> Node (framed "App" # centerXY) [go e1, go e2]
      LambdaBox n e   -> Node (hcat [framed "Lambda", framed n] # centerXY) [go e]
      where framed t = framedRoundText padding 0.9 size (txt size t)
            padding = 2 * txtHeight size

    renderT :: Tree (Diagram B) -> Diagram B
    renderT = drawTree
      where
        drawTree :: Tree (Diagram B) -> Diagram B
        drawTree = renderTree id (~~)
                 . symmLayout' (with & slWidth  .~ addGap (0   * size) . fromMaybe (0,0) . extentX
                                     & slHeight .~ addGap (0.5 * size) . fromMaybe (0,0) . extentY)
            where addGap gap = bimap (\x -> x - gap) (+ gap)

toDiagram :: Double -> ExpAsTree -> Diagram B
toDiagram size = renderT . flip runState (0, []) . go
  where
    go :: ExpAsTree -> State (Int, [(String, String)]) (Tree (Int, Diagram B))
    go = \case
      Box n           -> do i <- inc
                            let d = namedEnd i (framed n)
                            return $ Node (i, d) []
      BinaryBox e1 e2 -> do i <- inc
                            node1@(Node (e1Id, _) _) <- go e1
                            node2@(Node (e2Id, _) _) <- go e2
                            let d = namedEnd i $ hcat [nameIt i "e1" sbox, nameIt i "e2" sbox] # centerXY
                            modify (second (++ [(dName i "e1", endName e1Id),
                                                (dName i "e2", endName e2Id)]))
                            return $ Node (i, d) [node1, node2]
      LambdaBox n e   -> do i <- inc
                            node@(Node (eId, _) _) <- go e
                            let d = hcat [framed "λ", namedEnd i (framed n), nameIt i "e" sbox] # centerXY
                            modify (second ((dName i "e", endName eId) :))
                            return $ Node (i, d) [node]
      where sbox = rect w h
                where h = txtHeight size + padding
                      w = 0.7 * h
            framed t = framedText padding size (txt size t)
            padding = 2 * txtHeight size

            dName i suffix = show i ++ suffix
            endName i = dName i "to"
            nameIt i suffix d = d # named (dName i suffix)
            namedEnd i d = d # named (endName i)

            inc = gets fst <* modify (first succ)

    renderT :: (IsName n1, IsName n2) => (Tree (Int, Diagram B), (Int, [(n1, n2)])) -> Diagram B
    renderT (t, (_, connections)) = drawTree (fmap snd t) # applyAll (makeConnections connections)
      where
        drawTree :: Tree (Diagram B) -> Diagram B
        drawTree = renderTree' id (\_ _ -> mempty)
                 . symmLayout' (with & slWidth  .~ addGap (0   * size) . fromMaybe (0,0) . extentX
                                     & slHeight .~ addGap (0.5 * size) . fromMaybe (0,0) . extentY)
            where addGap gap = bimap (\x -> x - gap) (+ gap)

        makeConnections :: (IsName n1, IsName n2) => [(n1, n2)] -> [Diagram B -> Diagram B]
        makeConnections = map (\(n1, n2) -> connectPerim n1 n2 bottomSide topSide)

topSide, bottomSide :: Angle Double
topSide  = tau/4 @@ rad
bottomSide  = 3/4*tau @@ rad

txt :: Double -> String -> Diagram B
txt scaleFactor t = texterific t # scale scaleFactor

framedText :: Double -> Double -> Diagram B -> Diagram B
framedText spaceSize txtScaleFactor d = d # centerXY <> rect w h # fc white
  where w = spaceSize + width  d
        h = spaceSize + txtHeight txtScaleFactor

framedRoundText :: Double -> Double -> Double -> Diagram B -> Diagram B
framedRoundText spaceSize roundness txtScaleFactor d = d # centerXY <> roundedRect w h roundness # fc white
  where w = spaceSize + width  d
        h = spaceSize + txtHeight txtScaleFactor

txtHeight :: Double -> Double
txtHeight scaleFactor = height $ txt scaleFactor "a"


-------

t1 :: Tree Char
t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJKLM"), Node 'N' (map lf "OPQR")]]
  where lf x = Node x []

exampleSymmTree :: Diagram B
exampleSymmTree =
  renderTree ((<> circle 1) . text . (:[]))
             (~~)
             (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t1)
  # centerXY # pad 1.1

-------
