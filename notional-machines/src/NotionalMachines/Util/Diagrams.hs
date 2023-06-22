{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}

module NotionalMachines.Util.Diagrams where

import Data.List       (intersperse)
import Data.List.Split (chunksOf)

import Prettyprinter               (LayoutOptions (..), PageWidth (..), layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)

import Diagrams.Prelude   (Any, Colour, D, Diagram, Path, QDiagram, Renderable, SizeSpec, V2,
                           alignBR, alignT, boundingRect, centerX, centerXY, def, dims2D, fc,
                           fontSizeL, hcat, height, hrule, hsep, lw, lwO, mkWidth, none, rect, red,
                           sized, stroke, text, vcat, vrule, vsep, width, withEnvelope, (#))
import Diagrams.TwoD.Text (Text)

import Graphics.SVGFonts            (TextOpts (textFont), drop_rect, fit_height, set_envelope,
                                     svgText)
import Graphics.SVGFonts.PathInRect (PathInRect)
import Graphics.SVGFonts.ReadFont   (PreparedFont, loadFont)

import NotionalMachines.Lang.Error (Error)

import Paths_notional_machines (getDataFileName)


framed :: _ => Diagram b -> Diagram b
framed d = d <> boundingRect d # lwO 1

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

hSepRule :: _ => Double -> [QDiagram b V2 Double Any] -> QDiagram b V2 Double Any
hSepRule spc ds = hsep spc . intersperse (vrule (height ds) # lwO 0.5) $ ds

vSepRule :: _ => Double -> [QDiagram b V2 Double Any] -> QDiagram b V2 Double Any
vSepRule spc ds = vsep spc . intersperse (hrule (width ds) # lwO 0.5) $ ds


renderD :: _ => (FilePath -> SizeSpec V2 Double -> QDiagram b V2 Double Any -> IO ())
             -> FilePath
             -> Int
             -> IO (Either Error (Diagram b))
             -> IO ()
renderD renderer fileName w d = d >>=
    either print (renderer fileName (mkWidth (fromIntegral w)))

diagramWithError :: _ => IO (Either Error (QDiagram b V2 Double Any)) -> IO (QDiagram b V2 Double Any)
diagramWithError = fmap (either (d . s) id)
  where d = vcat . map (text' red 1) . lines
        s = renderString . layoutPretty layoutOptions . pretty
        layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine maxCharsPerLine 1.0 }
        maxCharsPerLine = 40

text', tightText :: _ => Colour Double
                      -> Double
                      -> String
                      -> QDiagram b V2 Double Any
text'     = _text def set_envelope
tightText = _text def (stroke . drop_rect)

text'' :: _ => PreparedFont Double
            -> Colour Double
            -> Double
            -> String
            -> QDiagram b V2 Double Any
text'' font = _text def { textFont = font } set_envelope

_text :: _ => TextOpts Double
           -> (PathInRect Double -> QDiagram b V2 Double Any)
           -> Colour Double
           -> Double
           -> String
           -> QDiagram b V2 Double Any
_text opts toDia c h s = s # svgText opts
                           # fit_height h
                           # toDia
                           # lw none
                           # fc c

fontMono :: IO (PreparedFont Double)
fontMono = loadFont =<< getDataFileName "data/fonts/DroidSansMonoDottedForPowerline.svg"
