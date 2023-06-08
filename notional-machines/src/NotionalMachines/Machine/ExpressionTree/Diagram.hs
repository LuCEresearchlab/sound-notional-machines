{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NotionalMachines.Machine.ExpressionTree.Diagram where

import Control.Monad.State (State, gets, modify, runState)

import Data.Bifunctor (bimap, first, second)
import Data.Maybe     (fromMaybe)
import Data.Monoid    (Any)
import Data.Tree      (Tree (Node))

import Diagrams.Prelude          (Angle, Colour, IsName, Path, QDiagram, Renderable, V2, applyAll,
                                  bgFrame, black, centerXY, circle, connectPerim', extentX, extentY,
                                  fc, gray, hcat, hsep, lc, lw, named, pad, rad, rect, roundedRect,
                                  shaftStyle, tau, text, thin, white, width, with, (#), (%~), (&),
                                  (.~), (@@), (~~))
import Diagrams.TwoD.Layout.Tree (renderTree, renderTree', slHSep, slHeight, slVSep, slWidth,
                                  symmLayout')
import Diagrams.TwoD.Text        (Text)

import NotionalMachines.Machine.ExpressionTree.Main (ExpAsTree (..))

import NotionalMachines.Util.Diagrams (tightText, vDiaSeq)

toDiagramBubble :: _ => ExpAsTree -> IO (QDiagram b V2 Double Any)
toDiagramBubble = return . _toDiagramBubble 1

toDiagramBubbleSeq :: _ => [ExpAsTree] -> IO (QDiagram b V2 Double Any)
toDiagramBubbleSeq = toDiagramSeq toDiagramBubble

toDiagramBoxes :: _ => ExpAsTree -> IO (QDiagram b V2 Double Any)
toDiagramBoxes = return . _toDiagramBoxes 1

toDiagramBoxesSeq :: _ => [ExpAsTree] -> IO (QDiagram b V2 Double Any)
toDiagramBoxesSeq = toDiagramSeq toDiagramBoxes

toDiagramSeq :: (Renderable (Path V2 Double) b, Renderable (Text Double) b, Monad f) => (a -> f (QDiagram b V2 Double Any)) -> [a] -> f (QDiagram b V2 Double Any)
toDiagramSeq f = fmap (vDiaSeq 1.5 0.5) . mapM f


bg :: _ => QDiagram b V2 Double Any -> QDiagram b V2 Double Any
bg = bgFrame 0.1 white

_toDiagramBubble :: _ => Double -> ExpAsTree -> QDiagram b V2 Double Any
_toDiagramBubble size = bg . renderT . go
  where
    go :: _ => ExpAsTree -> Tree (QDiagram b V2 Double Any)
    go = \case
      Box n           -> Node (framed n)     []
      BinaryBox e1 e2 -> Node (framed "App") [go e1, go e2]
      LambdaBox n e   ->
          Node (surround $ hsep (0.3 * padding)
                                [txt black size "λ" # centerXY, nameDef n]) [go e]
      where framed t = surround (txt black size t)
            surround = framedRoundText padding 0.2 size
            padding = 1.0 * txtHeight size
            nameDef t = framedText (0.3 * padding) (0.8 * size) (txt white size t)
                      # fc gray
                      # lc gray

    -- renderT :: Tree (QDiagram b V2 Double Any) -> QDiagram b V2 Double Any
    renderT = drawTree
      where
        -- drawTree :: Tree (QDiagram b V2 Double Any) -> QDiagram b V2 Double Any
        drawTree = renderTree id (\a b -> a ~~ b # lw thin)
                 . symmLayout' (with & slWidth  .~ addGap (0 * size) . fromMaybe (0,0) . extentX
                                     & slHeight .~ addGap (0 * size) . fromMaybe (0,0) . extentY)
            where addGap gap = bimap (+ (-gap)) (+ gap)

_toDiagramBoxes :: _ => Double -> ExpAsTree -> QDiagram b V2 Double Any
_toDiagramBoxes size = bg . renderT . flip runState (0, []) . go
  where
    go :: _ => ExpAsTree -> State (Int, [(String, String)]) (Tree (Int, QDiagram b V2 Double Any))
    go = \case
      Box n           -> do i <- inc
                            let d = namedEnd i (framed n)
                            return $ Node (i, d) []
      BinaryBox e1 e2 -> do i <- inc
                            node1@(Node (e1Id, _) _) <- go e1
                            node2@(Node (e2Id, _) _) <- go e2
                            let d = namedEnd i $ hcat [nameIt i "e1" sbox,
                                                       nameIt i "e2" sbox] # centerXY
                            modify (second (++ [(dName i "e1", endName e1Id),
                                                (dName i "e2", endName e2Id)]))
                            return $ Node (i, d) [node1, node2]
      LambdaBox n e   -> do i <- inc
                            node@(Node (eId, _) _) <- go e
                            let d = hcat [framed "λ", namedEnd i (framed n), nameIt i "e" sbox] # centerXY
                            modify (second ((dName i "e", endName eId) :))
                            return $ Node (i, d) [node]
      where sbox = framed ""
            framed t = framedText padding size (txt black size t)
            padding = 1.0 * txtHeight size

            dName i suffix = show i ++ suffix
            endName i = dName i "to"
            nameIt i suffix d = d # named (dName i suffix)
            namedEnd i d = d # named (endName i)

            inc = gets fst <* modify (first succ)

    renderT :: (IsName n1, IsName n2, _) => (Tree (Int, QDiagram b V2 Double Any), (Int, [(n1, n2)])) -> QDiagram b V2 Double Any
    renderT (t, (_, connections)) = drawTree (fmap snd t) # applyAll (makeConnections connections)
      where
        drawTree :: Tree (QDiagram b V2 Double Any) -> QDiagram b V2 Double Any
        drawTree = renderTree' id (\_ _ -> mempty)
                 . symmLayout' (with & slWidth  .~ addGap (0   * size) . fromMaybe (0,0) . extentX
                                     & slHeight .~ addGap (0.5 * size) . fromMaybe (0,0) . extentY)
            where addGap gap = bimap (\x -> x - gap) (+ gap)

        makeConnections :: (IsName n1, IsName n2, _) => [(n1, n2)] -> [QDiagram b V2 Double Any -> QDiagram b V2 Double Any]
        makeConnections = map (\(n1, n2) -> connectPerim' (with & shaftStyle %~ lw thin) n1 n2 bottomSide topSide)

topSide, bottomSide :: Angle Double
topSide  = tau/4 @@ rad
bottomSide  = 3/4*tau @@ rad

txt :: _ => Colour Double -> Double -> String -> QDiagram b V2 Double Any
txt = tightText

framedText :: _ => Double -> Double -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
framedText spaceSize txtScaleFactor d = d # centerXY <> rect w h # lw thin
  where w = spaceSize + width  d
        h = 0.5 * spaceSize + txtHeight txtScaleFactor

framedRoundText :: _ => Double -> Double -> Double -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
framedRoundText spaceSize roundness txtScaleFactor d =
    d # centerXY <> roundedRect w h roundness # fc white # lw thin
  where w = spaceSize + width  d
        h = 0.5 * spaceSize + txtHeight txtScaleFactor

txtHeight :: Double -> Double
txtHeight scaleFactor = 1.0 * scaleFactor


-------

t1 :: Tree Char
t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJKLM"), Node 'N' (map lf "OPQR")]]
  where lf x = Node x []

exampleSymmTree :: _ => QDiagram b V2 n Any
exampleSymmTree =
  renderTree ((<> circle 1) . text . (:[]))
             (~~)
             (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t1)
  # centerXY # pad 1.1

-------
