{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module NotionalMachines.Machine.AlligatorEggs.ColorAsName (
      Color (.., MkColorFromName)
    , colorHexa
    ) where

import Data.List  (elemIndex, unfoldr)
import Data.Maybe (fromJust)

import qualified Data.Colour                  as C (Colour)
import           Data.Colour.Palette.ColorSet (infiniteWebColors)
import           Data.Colour.RGBSpace         (uncurryRGB)
import           Data.Colour.RGBSpace.HSV     (RGB, hsv, hue)
import           Data.Colour.SRGB             (sRGB, sRGB24show, toSRGB)

------------------------------------------------------------------
-- Map from variable name to colour (and back) via a mapping from name to Int (and back)
------------------------------------------------------------------
-- Fundamentaly, the space of names is infinite, and the space of Colour Double
-- is not because Double is finite. Nevertheless, the conceptual point of the
-- mapping from Untyped Lambda Calculus and Alligator Eggs still stands (module
-- that limitation) so we want to find a mapping of names to colours that is
-- useful.
--
-- ps.: This would be easy with automatic backward function generation =====

--------------------------
-- Map from name to Int
--------------------------
nameToInt :: String -> Int
nameToInt = changeBase letterBase . map charToInt
intToName :: Int -> String
intToName = map intToChar . changeBaseInv letterBase

charToInt :: Char -> Int
charToInt = (\i -> i - fromEnum 'a') . fromEnum
intToChar :: Int -> Char
intToChar = toEnum . (\i -> i + fromEnum 'a')

changeBase :: Int -> [Int] -> Int
changeBase base = sum . zipWith (\i n -> n * base^i) [0::Integer ..]
changeBaseInv :: Int -> Int -> [Int]
changeBaseInv base = (\l -> if null l then [0] else l) . f
    where f = unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` base, b `div` base))

letterBase :: Int
letterBase = 26 -- letters are numbers in base 26


--------------------------
-- Map from Int to Color (v1)
--------------------------

colorToNatV1 :: C.Colour Double -> Int
colorToNatV1 = hueToInt defaultSpacing . rgbToHue . toSRGB
natToColorV1 :: Int -> C.Colour Double
natToColorV1 = uncurryRGB sRGB . hueToRGB . intToHue defaultSpacing

hueToRGB :: Double -> RGB Double
hueToRGB h = hsv h 1 1
rgbToHue :: RGB Double -> Double
rgbToHue = hue

nameToHue :: String -> Double
nameToHue = intToHue defaultSpacing . nameToInt
hueToName :: Double -> String
hueToName = intToName . hueToInt defaultSpacing

intToHue :: Int -> Int -> Double
intToHue spc x = fromIntegral (x `mod` spc) / fromIntegral spc * 360 + additive
  where additive = fromIntegral (x `div` spc)
  -- where additive = (360/fromIntegral spc) / fromIntegral (2^(x `div` spc))
hueToInt :: Int -> Double -> Int
hueToInt spc x = h x + defaultSpacing * (round x - round ((360 :: Double) / fromIntegral spc) * h x)
  where h y = round (fromIntegral defaultSpacing * (y / 360))

defaultSpacing :: Int
defaultSpacing = 6 -- space between one colour and the next

--------------------------
-- Another approach to map from Int to Color (v2)
--------------------------
-- The focus here is to generate colors that are convenient for visualization,
-- with good contrast and generally subjectvely pleasant to look at.

colorToNatV2 :: C.Colour Double -> Int
colorToNatV2 c = fromJust $ elemIndex c convenientColors
natToColorV2 :: Int -> C.Colour Double
natToColorV2 i = convenientColors !! i

-- This list is infinie so indexing into it is guaranteed
convenientColors :: [C.Colour Double]
convenientColors = takePeriod 13 $ tail infiniteWebColors
  where takePeriod :: Int -> [b] -> [b]
        takePeriod p l = map (\i -> l !! (i*p)) [0..]

------------------------------------------------------------------

--------------------------
-- Colors
--------------------------
-- Wrapping around colors convenient to map to/from names and with Enum
-- and Ord instances that make color substitution and recoloring simpler.

newtype Color = MkColor { colorContent :: C.Colour Double }
  deriving (Eq, Read)

pattern MkColorFromName :: String -> Color
pattern MkColorFromName c <- (MkColor ( intToName  . colorToNat -> c ))
  where MkColorFromName    =  MkColor . natToColor . nameToInt

{-# COMPLETE MkColorFromName #-}

instance Enum Color where
  toEnum = MkColor . natToColor
  fromEnum = colorToNat . colorContent

instance Ord Color where
  compare (MkColor c1) (MkColor c2) = compare (colorToNat c1) (colorToNat c2)

instance Show Color where
  show = colorHexa

colorToNat :: C.Colour Double -> Int
colorToNat = colorToNatV2

natToColor :: Int -> C.Colour Double
natToColor = natToColorV2

colorHexa :: Color -> String
colorHexa = sRGB24show . colorContent
--------------------------



