{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module NotionalMachines.Util.Diagrams where


import Data.List       (intersperse)
import Data.List.Split (chunksOf)

import           Diagrams.Backend.CmdLine            (DiagramLoopOpts (..), DiagramOpts (..),
                                                      mainRender)
import qualified Diagrams.Backend.Rasterific         as Rasterific (B)
import           Diagrams.Backend.Rasterific.CmdLine ()
import           Diagrams.Backend.SVG                (SVG, renderSVG)
import           Diagrams.Prelude                    hiding (dot, trace, uncons)
import           Diagrams.TwoD.Text                  (Text)

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

renderDiagramSVG :: FilePath -> Int -> QDiagram SVG V2 Double Any -> IO ()
renderDiagramSVG fileName w = renderSVG fileName (mkWidth (fromIntegral w))

-- Rendering with Rasterific
renderDiagramRaster :: FilePath -> Int -> Diagram Rasterific.B -> IO ()
renderDiagramRaster fileName w = mainRender dft
  where dft = (DiagramOpts (Just w) Nothing fileName, DiagramLoopOpts False Nothing 0)


