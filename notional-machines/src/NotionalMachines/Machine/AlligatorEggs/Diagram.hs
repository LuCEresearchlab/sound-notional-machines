{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

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

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude     (Diagram, alignT, centerX, centerXY, extrudeTop, hcat, mkWidth,
                             rotateBy, sized, (#), (===))
import Diagrams.SVG.ReadSVG (readSVGLBS)

import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamilyF (..))
import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color, colorHexa)
import NotionalMachines.Utils                      (replace)

-- | Returns the diagram of an alligator family.
toDiagram :: Double -> [AlligatorFamilyF Color] -> IO (Diagram B)
toDiagram width = fmap mconcat . toDiagram' width

-- | Returns the diagram of an alligator family as a list to allow for
-- post processing (e.g. to add a frame around each element,
-- which can be done by calling `map framed` before `mconcat`).
-- (See https://diagrams.github.io/doc/manual.html#delayed-composition)
toDiagram' :: Double -> [AlligatorFamilyF Color] -> IO [Diagram B]
toDiagram' maxWidth = fmap (hcat . fmap alignT) . mapM (uncurry go) . widths (maxWidth * 0.9)
  where
    go :: Double -> AlligatorFamilyF Color -> IO [Diagram B]
    go w (Egg c)                 = (\e -> [e] # sized (mkWidth (w * 0.9))) <$> egg (colorHexa c)
    go w (OldAlligator as')      = coverWith w oldAlligator as'
    go w (HungryAlligator c as') = coverWith w (hungryAlligator (colorHexa c)) as'

    coverWith w alligator as' = do proteges <- toDiagram' w as'
                                   al <- alligator
                                   return $ [al] # sized (mkWidth w) # centerX
                                             ===
                                           proteges # centerX

    -- | Returns the width of each alligator family.
    -- The widths are calculated so that:
    -- - the sum of the widths of the families is equal to the width of the
    --   alligator that is protecting them.
    -- - the height of an egg is equal to the height of an alligator.
    --
    -- The full explanation is in the comments at the end of this file.
    widths :: Double -> [AlligatorFamilyF c] -> [(Double, AlligatorFamilyF c)]
    widths w _as = map (\a -> (width a, a)) _as
      where count f = (fromIntegral . length . filter f) _as
            numAlligators = count (not . isEgg)
            numEggs = count isEgg

            isEgg = \case Egg {} -> True
                          _      -> False

            width :: AlligatorFamilyF c -> Double
            width a = if isEgg a
                      then w / (numEggs       + numAlligators * spriteHeightRatio)
                      else w / (numAlligators + numEggs       * (1.0 / spriteHeightRatio))

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
