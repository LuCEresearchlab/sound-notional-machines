{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}


{-|
Description : Render Alligator Eggs using @diagrams@.
Stability   : experimental

To render images you can run the repl:
$ stack repl

Import a few packages:
>>> import NotionalMachines.Lang.UntypedLambda.Main as UL
>>> import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs as ULAl

Render an image with Alligator for the lambda term `\s.\z.s(s z)` in a file
called `alligators.svg` with 400 pixels of width
>>> either print (renderAlligators "alligators.svg" 400 . ULAl.langToNm) $ UL.parse "\\s.\\z.s(s z)"

-}

module NotionalMachines.Machine.AlligatorEggs.Diagram where

import Data.String (fromString)

import Diagrams.Prelude     (Any, Default (def), QDiagram, V2, alignT, bgFrame, centerX, centerXY,
                             extrudeTop, hcat, mkWidth, rotateBy, sized, white, (#), (===))
import Diagrams.SVG.ReadSVG (readSVGLBS)

import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color, colorHexa)
import NotionalMachines.Machine.AlligatorEggs.Main        (AlligatorFamilyF (..))

import NotionalMachines.Util.Util (replace)

import Paths_notional_machines (getDataFileName)


data AlligatorOpts b = AlligatorOpts { _maxWidth :: Double
                                       -- ^ Maximum width of the diagram.
                                     , _f :: QDiagram b V2 Double Any -> QDiagram b V2 Double Any
                                       -- ^ Function to apply to each alligator or egg
                                       -- (e.g. `framed` to ddd a frame around each)
                                     , _widths :: Double -> [AlligatorFamilyF Color] -> [(Double, AlligatorFamilyF Color)]
                                       -- ^ Function to adjust the widths of the alligator families.
                                       -- Given the with of a protector and a list proteges,
                                       -- determine the width of each protege.
                                     , _framePadding :: Double
                                     }

instance Default (AlligatorOpts b) where
    def = AlligatorOpts 1 id adjustedWidths 0.05

toDiagram :: _ => [AlligatorFamilyF Color] -> IO (QDiagram b V2 Double Any)
toDiagram = toDiagram' def

-- | Returns the diagram of an alligator family.
toDiagram' :: _ => AlligatorOpts b -> [AlligatorFamilyF Color] -> IO (QDiagram b V2 Double Any)
toDiagram' opts = fmap (bg . mconcat . map (_f opts)) . _toDiagram opts
  where bg = bgFrame (_framePadding opts) white

-- | Returns the diagram of an alligator family as a list to allow for
-- post processing (e.g. to add a frame around each element,
-- which can be done by calling `map framed` before `mconcat`).
-- (See https://diagrams.github.io/doc/manual.html#delayed-composition)
_toDiagram :: _ => AlligatorOpts b -> [AlligatorFamilyF Color] -> IO [QDiagram b V2 Double Any]
_toDiagram opts = fmap (hcat . fmap alignT)
                . mapM (uncurry go)
                . _widths opts (_maxWidth opts * 0.9)
  where
    -- go :: _ => Double -> AlligatorFamilyF Color -> IO [QDiagram b V2 Double Any]
    go w (Egg c)                 = (\e -> [e] # sized (mkWidth (w * 0.9))) <$> egg (colorHexa c)
    go w (OldAlligator as')      = coverWith w oldAlligator as'
    go w (HungryAlligator c as') = coverWith w (hungryAlligator (colorHexa c)) as'

    coverWith w alligator as' = do proteges <- _toDiagram (opts { _maxWidth = w }) as'
                                   al <- alligator
                                   return $ [al] # sized (mkWidth w) # centerX
                                             ===
                                           proteges # centerX

adjustedWidths, equalWidths, constWidths :: Double -> [AlligatorFamilyF c] -> [(Double, AlligatorFamilyF c)]

equalWidths w _as = map (\a -> (width a, a)) _as
    where width _ = w / (fromIntegral . length) _as

constWidths w = map (\a -> (width a, a))
    where width = \case Egg {} -> w / 2
                        _      -> w

-- | Returns the width of each alligator family.
-- The widths are calculated so that:
-- - the sum of the widths of the families is equal to the width of the
--   alligator that is protecting them.
-- - the height of an egg is equal to the height of an alligator.
--
-- The full explanation is in the comments at the end of this file.
adjustedWidths w _as = map (\a -> (width a, a)) _as
    where width :: AlligatorFamilyF c -> Double
          width a = if isEgg a
                    then w / (numEggs       + numAlligators * spriteHeightRatio)
                    else w / (numAlligators + numEggs       * (1.0 / spriteHeightRatio))
              where count f = (fromIntegral . length . filter f) _as
                    numAlligators = count (not . isEgg)
                    numEggs = count isEgg
                    isEgg = \case Egg {} -> True
                                  _      -> False

--------------------------
-- Sprites
--------------------------
egg :: _ => String -> IO (QDiagram b V2 Double Any)
egg color = sprite "egg" (Just color)

oldAlligator :: _ => IO (QDiagram b V2 Double Any)
oldAlligator = extrudeTop (-45) <$> sprite "oldAlligator" Nothing

hungryAlligator :: _ => String -> IO (QDiagram b V2 Double Any)
hungryAlligator color = rotateBy 0.5 <$> sprite "hungryAlligator" (Just color)

sprite :: _ => String -> Maybe String -> IO (QDiagram b V2 Double Any)
sprite name mColor = fmap centerXY
                   . (=<<) readSVGLBS
                   . fmap (fromString . changeColor mColor)
                   . loadSVG
                   $ name
    where changeColor = maybe id (\c -> replace "fill:red" ("fill:" ++ c))

loadSVG :: String -> IO String
loadSVG name = readFile =<< getDataFileName ("data/sprites/" ++ name ++ ".svg")

-- Height/Width of each sprite
spriteHeightRatio :: Double
spriteHeightRatio = eggHeightRatio/alligatorHeightRatio
    where eggHeightRatio = 0.6923
          alligatorHeightRatio = 0.4049

--------------------------
{-

How to calculate the with of each alligator family?

If an alligator with width `a` is protecting a list of families `fs` then the
sum of the widths of the families in `fs` must be `a`.
The first attempt was to equally distribute the width of the alligator among
the families in `fs`.
The problem is that the height of an egg may end up being bigger than the
height of an alligator.

To understand the solution to this consider the following example:

```
                a
      +--------------------+
      |                    |
      |                    |
      |         A          | a*a_p
      |                    |
      |                    |
      +-------------+------+
      |             |      |
b*b_p |      B      |   C  | c*c_p
      |             |      |
      +-------------+------+
             b          c
```

A and B are alligators and C is an egg.
A is protecting B and C.
The width of A is `a`, the width of B is `b` and the width of C is `c`.

The ratio between the width and the height of each sprite is fixed,
`a_p`, `b_p` and `c_p` respectively.

So if we have `n` alligators and `m` eggs in the list `fs` then our two constraints are:
- The sum of the widths of the families in `fs` must be `a`.
- The height of an egg must be equal to the height of an alligator.

These constraints are represented by the following equations (AsciiMath notation):

`{(n*b + m*c = a),
  (b*b_p = c*c_p):}`

Isolating `b` and `c` in the second equation:

`b=(c*c_p)/b_p` and `c=(b*b_p)/c_p`

Substituting `b` in the first equation:

`n*((c*c_p)/b_p)+m*c=a`

Isolating `c`:

`c=a/(n*((c_p)/b_p)+m)`

Substituting `c` in the first equation:

`n*b+m*((b*b_p)/c_p)=a`

Isolating `b`:

`b=a/(n+m*((b_p)/c_p))`

See good example with:
Alligator> :render (\a. (\x.a) a ((\a.a) c)) (\b.b)
Alligator> :render (\a. b b b b b b b b b b b)

-}
