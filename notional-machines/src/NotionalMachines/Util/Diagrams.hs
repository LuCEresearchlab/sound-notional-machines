{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}

module NotionalMachines.Util.Diagrams where


import Data.List       (intersperse)
import Data.List.Split (chunksOf)

import Diagrams.Prelude (Any, D, Diagram, Enveloped, Path, QDiagram, Renderable, SizeSpec,
                         TrailLike, Transformable, V, V2, alignBR, alignT, boundingRect, centerX,
                         centerXY, dims2D, fontSizeL, hcat, height, hrule, lw, lwO, mkWidth, rect,
                         sized, text, vcat, vrule, vsep, width, withEnvelope, (#))

import Diagrams.TwoD.Text (Text)

import NotionalMachines.Lang.Error (Error)

framed :: (Enveloped d, Transformable d, TrailLike d, Monoid d, V d ~ V2) => d -> d
framed d = d <> boundingRect d

diaSeq :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) =>
          Int -> Double -> Double -> [QDiagram b V2 Double Any] -> QDiagram b V2 Double Any
diaSeq n w h =      hcat . map alignT . (\ds -> intersperse (vrule (height ds)) ds)
             . map (vcat .              (\ds -> intersperse (hrule (width  ds)) ds))
             . chunksOf n
             . zipWith (addIndex 0.9) [(0 :: Integer)..]
             . map withSpacing
  where withSpacing = withEnvelope (rect w h :: D V2 Double) . centerXY . sized (dims2D (0.9 * w) (0.9 * h))
        rectPerc p d = rect (p * width d) (p * height d) # lw 0
        addIndex perc i d = d <> (rectPerc perc d # alignBR <> idx i) # centerXY
          where idx j = rectPerc (1-perc) d <> text (show j) # fontSizeL ((1-perc) * height d)

vDiaSeq :: (Renderable (Path V2 Double) b, Renderable (Text Double) b) =>
           Double -> Double -> [QDiagram b V2 Double Any] -> QDiagram b V2 Double Any
vDiaSeq spc fontS = vsep spc
       . zipWith (addIndex spc fontS) [(0 :: Integer)..]
       . (\ds -> map (\d -> vsep spc [d # centerX, hrule (maxWidth ds) # lwO 1]) ds)
  where maxWidth = maximum . map width
        addIndex _spc _fontS i d = d # centerXY <> (innerRect # alignBR <> idx i) # centerXY
          where innerRect = rect (width d - _spc) (height d - _spc) # lwO 0
                idx j = text (show j) # fontSizeL _fontS

renderD :: _ => (FilePath -> SizeSpec V2 Double -> QDiagram b V2 Double Any -> IO ())
             -> FilePath
             -> Int
             -> IO (Either Error (Diagram b))
             -> IO ()
renderD renderer fileName w d = d >>=
    either print (renderer fileName (mkWidth (fromIntegral w)))

