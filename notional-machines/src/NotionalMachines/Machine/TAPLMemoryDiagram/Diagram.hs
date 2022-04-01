{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Diagram (
    toDiagram
  ) where
import Control.Monad.State.Lazy ( runState, withState, MonadState(get), State )

import           Data.Map (Map)
import qualified Data.Map as Map

import Prettyprinter (Pretty (pretty))

import Diagrams.Prelude (Diagram, IsName, _arrowHead, _arrowShaft, _headLength, (#), centerXY, rect, roundedRect, width, height, circle, hcat, named, fc, black, vsep, (|||), (===), (@@), straightShaft, rad, arc, tau, xDir, turn, applyAll, def, connectPerim', fullTurn, vcat, strutX, strutY, spike, scale, normalized, local, centerX, Applicative (liftA2), ArrowOpts (_headGap), connectOutside', alignB, lw)
import Diagrams.Backend.Rasterific.CmdLine ( B )
import Diagrams.Backend.Rasterific.Text ( texterific )

import NotionalMachines.Machine.TAPLMemoryDiagram.Main (TAPLMemoryDiagram(..), DTerm(..), DLocation(..))

---------------
-- Tree Heap diagram
---------------

type Name = String

type ArrowInfo l = (Int, DLocation l, Connector l)
type Connector l = Int -> DLocation l -> Diagram B -> Diagram B

instance IsName a => IsName (DLocation a)

toDiagram :: forall l. (IsName l) => Double -> TAPLMemoryDiagram l -> Diagram B
toDiagram ts taplDia = let (d, arrowLocs) = runState (allDia taplDia) []
                        in d # applyAll (map connect arrowLocs)
  where
    rightSideOfStore = 0     @@ rad
    leftSideOfStore  = tau/2 @@ rad

    curvedShaft = arc xDir (1/2 @@ turn)

    connectToLeft  l1 l2 = connectPerim'   (arrowConfig straightShaft) l1 l2 fullTurn leftSideOfStore
    connectToRight l1 l2 = connectPerim'   (arrowConfig curvedShaft)   l1 l2 fullTurn rightSideOfStore
    connectAnywhere      = connectOutside' (arrowConfig straightShaft)

    arrowConfig shaft = def { _arrowShaft = shaft,
                              _headLength = local (ts * 0.07) `atLeast` normalized 0.01,
                              _headGap    = local (ts * 0.03) }
              where atLeast = liftA2 max

    connect :: ArrowInfo l -> Diagram B -> Diagram B
    connect (l1, l2, connector) = connector l1 l2

    allDia :: TAPLMemoryDiagram l -> State [ArrowInfo l] (Diagram B)
    allDia (TAPLMemoryDiagram term nameEnv store) =
      do t <- valDia connectAnywhere term
         env <- nameEnvDia nameEnv
         s <- storeDia store
         return $          t # centerX
                          ===
                    strutY (ts * 0.1)
                          ===
           (env ||| strutX (ts * 0.2) ||| s) # centerX

    nameEnvDia :: Map Name (DTerm l) -> State [ArrowInfo l] (Diagram B)
    nameEnvDia = fmap ((<>) (strutX (ts * 0.5)) . frameStack) . mapM draw . Map.toList
      where draw :: (Name, DTerm l) -> State [ArrowInfo l] (Diagram B, Diagram B)
            draw (name, val) = (txt (ts * 0.1) name, ) <$> valDia connectToLeft val
            frameStack :: [(Diagram B, Diagram B)] -> Diagram B
            frameStack stack = vcat $ map frameSlot stack
              where frameSlot (nD, vD) = boxed ((ts * 0.2) + maxWidth fst) (h nD) nD
                                     ||| boxed ((ts * 0.2) + maxWidth snd) (h nD) vD
                    maxWidth f = maximum (map (width . f) stack)
                    h = (+ (ts * 0.2)) . height

    storeDia :: Map (DLocation l) (DTerm l) -> State [ArrowInfo l] (Diagram B)
    storeDia = fmap (vsep 0.8) . mapM draw . Map.toList
      where draw (loc, val) = named loc . framedRound (ts * 0.2) <$> valDia connectToRight val

    valDia :: Connector l -> DTerm l -> State [ArrowInfo l] (Diagram B)
    valDia _         (Leaf v)    = (return . txt (ts * 0.1) . show . pretty) v
    valDia _         (Branch [Leaf "$nat", Leaf " ", Leaf n]) = (return . txt (ts * 0.1) . show . pretty) n
    valDia connector (Branch xs) = hcat <$> mapM (valDia connector) xs
    valDia connector (TLoc l)    = do newId <- fmap nextId get
                                      withState ((newId, l, connector) :) (return (refOrigin newId))
      where nextId [] = 0
            nextId ((i,_,_):_) = succ i

            refOrigin newId = (point # named newId <> rect (ts * 0.1) (txtHeight (ts * 0.1)) # lw 0) # alignB

point :: Diagram B
point = circle 0.05 # fc black

-- "boxed" text (with dimentions)
txt :: Double -> String -> Diagram B
txt scaleFactor t = texterific t # scale scaleFactor

txtHeight :: Double -> Double
txtHeight scaleFactor = height $ txt scaleFactor "a" 

boxed :: Double -> Double -> Diagram B -> Diagram B
boxed w h d = d # centerXY <> rect w h

framedRound :: Double -> Diagram B -> Diagram B
framedRound spaceSize d = d # centerXY <> roundedRect w h r
  where
    r = 0.9
    w = spaceSize + width  d
    h = spaceSize + height d


