{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}

module NotionalMachines.Examples.Diagrams where

import Control.Monad.Except (ExceptT (..), lift, runExceptT)

import Prettyprinter (pretty)

import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import Diagrams.Prelude            (Any, Default (def), Diagram, QDiagram, SizeSpec, V2, black,
                                    centerX, centerXY, fontSizeL, hsep, lw, mkWidth, none, rect,
                                    sized, text, vsep, (#))

import Graphics.SVGFonts.ReadFont (PreparedFont)

import           NotionalMachines.Lang.Error              (Error)
import qualified NotionalMachines.Lang.UntypedLambda.Main as Lambda

import NotionalMachines.Machine.AlligatorEggs.Diagram  (AlligatorOpts (_widths), _f, constWidths,
                                                        equalWidths, toDiagram')
import NotionalMachines.Machine.ExpressionTree.Diagram (toDiagramBubble)

import           NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs  (diagramTrace, str2NM)
import qualified NotionalMachines.LangInMachine.UntypedLambdaExpressionTree as ETree

import NotionalMachines.Meta.Steppable (trace)

import NotionalMachines.Util.Diagrams (diagramWithError, fontMono, framed, hSepRule, text'')

-- renderD renderSVG "a.svg" 400 $ A.diagram "(\\a.a) c"
-- renderD renderCairo "a.pdf" 400 $ ETree.diagram toDiagramBubble "(\\a.a) c"
-- renderD renderCairo "a.pdf" 400 $ ETree.diagram toDiagramBoxes "(\\a.a) c"
-- renderD renderCairo "a.pdf" 400 $ LambdaRefTAPLDia.diagramTrace TAPLDia.termToTextDiagram "(\\x: Ref Nat. { x, x }) (ref 0)"
-- renderCairo "a.pdf" (mkWidth 400) =<< (diagramWithError $ LambdaRefTAPLDia.diagramTrace TAPLDia.termToTextDiagram "(\\x: Ref Nat.  x, x }) (ref 0)")
-- renderCairo "a.pdf" (mkWidth 400) =<< dia


render :: _ => (FilePath -> SizeSpec V2 Double -> QDiagram b V2 Double Any -> IO ())
             -> FilePath
             -> Int
             -> IO (Diagram b)
             -> IO ()
render renderer fileName w d = d >>=
    renderer fileName (mkWidth (fromIntegral w))

renderExample :: IO (QDiagram Rasterific V2 Double Any) -> IO ()
renderExample = render renderRasterific "a.pdf" 400

renderEitherExample :: IO (Either Error (QDiagram Rasterific V2 Double Any)) -> IO ()
renderEitherExample = renderExample . diagramWithError


alligatorWrongWidthsExample :: IO ()
-- alligatorWrongWidthsExample = renderEitherExample $ alligatorWrongWidths "(\\a. (\\x.a) a ((\\a.a) c)) (\\b.b)"
alligatorWrongWidthsExample = renderEitherExample $ alligatorWrongWidths
                              "(\\a.(\\b.b) (\\c.c) (\\d.d)) (\\e.(\\f.f) e)"


alligatorFixedWidthsExample :: _ => IO ()
alligatorFixedWidthsExample = do d1 <- diagramWithError (alligatorFixedWidths e)
                                 d2 <- diagramWithError (alligatorWrongWidths e)
                                 r d1 d2
    where e = "(\\a.(\\b.b) (\\c.c) (\\d.d)) (\\e.(\\f.f) e)"
          r d1 d2 = renderExample
            . return
            . hSepRule 0.1
            . map (sized (mkWidth 1) . centerXY)
            $ [d1, d2]


alligatorWrongWidths :: _ => String -> IO (Either Error (QDiagram b V2 Double Any))
alligatorWrongWidths = mapM (toDiagram' (def { _widths = equalWidths, _f = framed })) . str2NM

alligatorFixedWidths :: _ => String -> IO (Either Error (QDiagram b V2 Double Any))
alligatorFixedWidths = mapM (toDiagram' (def { _widths = constWidths })) . str2NM


alligatorTraceExample :: IO ()
alligatorTraceExample = renderEitherExample $ alligatorTrace "(\\t. \\f. t) a b"

alligatorTrace :: _ => String -> IO (Either Error (QDiagram b V2 Double Any))
alligatorTrace e = runExceptT
    (do alligators <- (ExceptT . diagramTrace) e
        terms <- (ExceptT . return . fmap trace . Lambda.parse) e
        font <- lift fontMono
        return . hsep 0.1 $ zipWith3 (stepInTaace font) [0..] terms alligators)

stepInTaace :: _ => PreparedFont Double -> Integer -> Lambda.Exp -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
stepInTaace font i e d = vsep 0.1 . map centerX $ [s i, p e, d]
  where txt t = text t # fontSizeL 0.1
             <> rect 0.2 0.2 # lw none
        txt' = text'' font black 0.2
        s = txt . ("step " ++) . show
        p = txt' . show . pretty


bubbleWithError :: _ => IO (QDiagram b V2 Double Any)
bubbleWithError =
    (diagramWithError . ETree.diagram toDiagramBubble) "(\\a.a) b"

