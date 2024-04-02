{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}

module NotionalMachines.Machine.ListAsStackOfBoxes.Diagram where

import Diagrams.Prelude (Any, QDiagram, V2, frame, fontSizeL, rect, text, (===), (#))

import NotionalMachines.Machine.ListAsStackOfBoxes.Main
    ( Stack(..), Box(..) )
import NotionalMachines.Util.Diagrams ()

-- | Convert a "List as stack of boxes" to a diagram.
toDiagram :: _ => Stack -> QDiagram b V2 Double Any
toDiagram = frame 0.1 . toDiagram'

toDiagram' :: _ => Stack -> QDiagram b V2 Double Any
toDiagram' Pallet      = rect 1.0 0.2
toDiagram' (Stack b s) = box b
                         ===
                     toDiagram' s
  where box :: _ => Box -> QDiagram b V2 Double Any
        box (Box a) = rect 0.5 0.5 <> text (show a) # fontSizeL 0.1
        -- pallet :: QDiagram b V2 Double Any
        -- pallet = rect 2.0 0.5

