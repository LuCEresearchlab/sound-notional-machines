{-# OPTIONS_GHC -Wall #-}

{-|
Description : Render Alligator Eggs using @diagrams@.
Stability   : experimental

To render images you can run the repl:
$ stack repl

Import a few packages:
>>> import NotionalMachines.Lang.UntypedLambda.Main as UL
>>> import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs as ULAl

Render an image with Alligator for the lambda term `\s.\z.s(s z)` in a file called `alligators.svg` with 400 pixels of width
>>> either print (renderAlligators "alligators.svg" 400 . ULAl.langToNm) $ UL.parse "\\s.\\z.s(s z)"

-}

module NotionalMachines.Machine.AlligatorEggs.Diagram where

import Data.String (fromString)

import Diagrams.Prelude (Diagram, (#), centerX, (===), alignT, mkWidth, centerXY, extrudeTop, rotateBy, hcat, sized)
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.SVG.ReadSVG (readSVGLBS)

import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamilyF (..), Color, colorHexa)
import NotionalMachines.Utils (replace, diaSeq)

-- | Returns the diagram of an alligator family.
toDiagram :: Double -> [AlligatorFamilyF Color] -> IO (Diagram B)
toDiagram size = fmap mconcat . toDiagram' size

-- | Returns the diagram of an alligator family as a list to allow for post processing (e.g. to add a frame arrow each element). (See https://diagrams.github.io/doc/manual.html#delayed-composition)
toDiagram' :: Double -> [AlligatorFamilyF Color] -> IO [Diagram B]
toDiagram' size as = (fmap (hcat . fmap alignT) . mapM (go scaleFactor)) as
  where
     scaleFactor = (size / fromIntegral (length as)) * 0.9

     go :: Double -> AlligatorFamilyF Color -> IO [Diagram B]
     go s (Egg c) = (\e -> [e] # sized (mkWidth (s * 0.7))) <$> egg (colorHexa c)
     go s (OldAlligator as') = coverWith s oldAlligator as'
     go s (HungryAlligator c as') = coverWith s (hungryAlligator (colorHexa c)) as'

     coverWith s alligator as' = do proteges <- toDiagram' s as'
                                    al <- alligator
                                    return $ [al] # sized (mkWidth s) # centerX
                                              ===
                                            proteges # centerX

--------------------------
-- Sprites
--------------------------
egg :: String -> IO (Diagram B)
egg color = sprite "egg" (Just color)

oldAlligator :: IO (Diagram B)
oldAlligator = extrudeTop (-45) <$> sprite "oldAlligator" Nothing

hungryAlligator :: String -> IO (Diagram B)
hungryAlligator color = rotateBy 0.5 <$> sprite "hungryAlligator" (Just color)

sprite :: String -> Maybe String -> IO (Diagram B)
sprite name mColor = ( fmap centerXY
                     . (=<<) readSVGLBS
                     . fmap (fromString . maybe id (\c -> replace "fill:red" ("fill:" ++ c)) mColor)
                     . readFile )
                     ("sprites/" ++ name ++ ".svg")