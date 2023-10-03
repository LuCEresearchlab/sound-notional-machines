{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NotionalMachines.Machine.ExpressionTree.BubbleDiagram where


import Data.Bifunctor (bimap)
import Data.Monoid    (Any)
import Data.Tree      (Tree (Node))

import Diagrams.Prelude          (Colour, Default (def), QDiagram, V2, bgFrame, black, centerXY,
                                  circle, fc, gray, hsep, lc, lw, pad, rect, roundedRect, text,
                                  thin, white, width, with, (#), (&), (.~), (~~))
import Diagrams.TwoD.Layout.Tree (renderTree, slHSep, slVSep, symmLayout')

import NotionalMachines.Machine.ExpressionTree.Main (ExpAsTree (..))

import NotionalMachines.Util.Diagrams (tightText)


data DiagramBubbleOpts = DiagramBubbleOpts { _fontSize     :: Double
                                           , _framePadding :: Double
                                           }

instance Default DiagramBubbleOpts where
    def = DiagramBubbleOpts 1 0.1

toDiagram :: _ => ExpAsTree -> IO (QDiagram b V2 Double Any)
toDiagram = toDiagram' def

toDiagram' :: _ => DiagramBubbleOpts -> ExpAsTree -> IO (QDiagram b V2 Double Any)
toDiagram' opts = return . _toDiagram opts

_toDiagram :: _ => DiagramBubbleOpts -> ExpAsTree -> QDiagram b V2 Double Any
_toDiagram opts = bg . renderT . go
  where
    size = _fontSize opts
    bg = bgFrame (_framePadding opts) white
    go :: _ => ExpAsTree -> Tree (QDiagram b V2 Double Any)
    go = \case
      Box n           -> Node (framed n)     []
      BinaryBox e1 e2 -> Node (framed "App") [go e1, go e2]
      LambdaBox n e   ->
          Node (surround $ hsep (0.3 * padding)
                                [txt black size "λ" # centerXY, nameDef n]) [go e]
      where framed t = surround (txt black size t)
            surround = framedRoundText padding (0.2 * size) size
            padding = 1.0 * txtHeight size
            nameDef t = framedText (0.3 * padding) (0.8 * size) (txt white size t)
                      # fc gray
                      # lc gray

    -- renderT :: Tree (QDiagram b V2 Double Any) -> QDiagram b V2 Double Any
    renderT = drawTree
      where
        -- drawTree :: Tree (QDiagram b V2 Double Any) -> QDiagram b V2 Double Any
        drawTree = renderTree id (\a b -> a ~~ b # lw thin)
                 . symmLayout' (with & slHSep .~ (2.5 * size)
                                     & slVSep .~ (2.2 * size))
            where addGap gap = bimap (+ (-gap)) (+ gap)

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


