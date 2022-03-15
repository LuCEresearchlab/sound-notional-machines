{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Diagram (
    mainStackAndHeap
  , toDiagram
  ) where
-- import Data.String (fromString)

-- import Diagrams.Prelude (Diagram, (#), centerX, (===), alignT, mkWidth, centerXY, extrudeTop, rotateBy, hcat, sized)
-- import Diagrams.Backend.SVG (B, renderSVG)
-- import Diagrams.SVG.ReadSVG (readSVGLBS)

-- import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamilyF (..), Color, colorHexa)
-- import NotionalMachines.Utils (replace, diaSeq)
-- import NotionalMachines.Machine.TAPLMemoryDiagram.Main (TAPLMemoryDiagram, Name)

import Control.Monad.State.Lazy

import           Data.Map (Map)
import qualified Data.Map as Map

import Data.Typeable (Typeable)

import Prettyprinter (Pretty (pretty))

import Diagrams.Prelude (Diagram, Angle, Trail, V2, D, IsName, _arrowShaft, (#), centerXY, rect, roundedRect, width, height, circle, hcat, named, fc, black, phantom, vsep, (|||), (@@), straightShaft, rad, arc, tau, xDir, turn, hsep, applyAll, def, connectPerim', fullTurn, vcat, bgFrame, white, strutX, _arrowHead, spike)
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific.Text

import NotionalMachines.Machine.TAPLMemoryDiagram.Main (TAPLMemoryDiagram(..), DTerm(..), DLocation(..))

---------------
-- Tree Heap diagram
---------------

type Name = String

data Val = Val Int | Loc Loc

newtype Loc = MkLoc Int
  deriving (Typeable, Eq, Ord, Show)
instance IsName Loc

type Stack = [(Name, Val)]
stackEx :: Stack
stackEx = zip (map (take 10 . repeat) ['a'..]) [Val 2, Loc (MkLoc 0), Val 5, Loc (MkLoc 3), Val 10, Val 2222]

type Heap = [(Loc, Val)]
heapEx :: Heap
heapEx = zip (map MkLoc [0..]) [Val 5, Val 6, Val 7, Loc (MkLoc 2), Val 99999]

---------------

type ArrowInfo = (Int, Loc, Angle Double, Trail V2 Double)

-- stack & heap
stackAndHeap :: Stack -> Heap -> Diagram B 
stackAndHeap stack heap = hsep 2 ds # applyAll (map connector arrowLocs)
  where
    connector (l1, l2, angle, shaft) = connectPerim' (def { _arrowShaft = shaft }) l1 l2 fullTurn angle
    curvedShaft = arc xDir (1/2 @@ turn)

    (ds, arrowLocs) = runState (sequence [stackDia stack, heapDia heap]) []

    stackDia :: Stack -> State [ArrowInfo] (Diagram B) 
    stackDia = fmap frameStack . mapM draw
      where draw :: (Name, Val) -> State [ArrowInfo] (Diagram B, Diagram B)
            draw (name, val) = (txt name, ) <$> valDia val (tau/2 @@ rad) straightShaft
            frameStack :: [(Diagram B, Diagram B)] -> Diagram B
            frameStack s = vcat $ map frameSlot s
              where frameSlot (nD, vD) = boxed (1 + maxWidth fst) (h nD) nD
                                     ||| boxed (1 + maxWidth snd) (h nD) vD
                    maxWidth f = maximum (map (width . f) s)
                    h _ = 2.5

    heapDia :: Heap -> State [ArrowInfo] (Diagram B)
    heapDia = fmap (vsep 0.8) . mapM draw
      where draw (loc, val) = named loc . framedRound <$> valDia val (0 @@ rad) curvedShaft

    valDia :: Val -> Angle Double -> Trail V2 Double -> State [ArrowInfo] (Diagram B)
    -- valDia _ = treeDia
    valDia (Val v) _     _     = return $ txt (show v)
    valDia (Loc v) angle shaft = do newId <- fmap nextId get
                                    withState ((newId, v, angle, shaft) :) (return $ point # named newId <> phm 0.5)
      where nextId [] = 0
            nextId ((i,_,_,_):_) = succ i

mainStackAndHeap :: Diagram B
mainStackAndHeap = bgFrame 1 white $ stackAndHeap stackEx heapEx
-------------

type ArrowInfo2 l = (Int, DLocation l, Angle Double, Trail V2 Double)
instance IsName a => IsName (DLocation a)

toDiagram :: forall l. (IsName l) => TAPLMemoryDiagram l -> Diagram B
toDiagram taplDia = let (d, arrowLocs) = runState (allDia taplDia) []
                     in d # applyAll (map connector arrowLocs)
  where
    leftSideOfStore = tau/2 @@ rad
    rightSideOfStore = 0 @@ rad
    topSideOfStore = tau/4 @@ rad

    curvedShaft = arc xDir (1/2 @@ turn)

    connector (l1, l2, angle, shaft) = connectPerim' arrowConfig l1 l2 fullTurn angle
      where arrowConfig = def { _arrowShaft = shaft, _arrowHead = spike }

    allDia :: TAPLMemoryDiagram l -> State [ArrowInfo2 l] (Diagram B)
    allDia (TAPLMemoryDiagram term nameEnv store) = do t <- valDia topSideOfStore straightShaft term
                                                       env <- nameEnvDia nameEnv
                                                       s <- storeDia store
                                                       return $ vsep 2 [t, hsep 2 [env, s]]

    nameEnvDia :: Map Name (DTerm l) -> State [ArrowInfo2 l] (Diagram B) 
    nameEnvDia = fmap ((<>) (strutX 10) . frameStack) . mapM draw . Map.toList
      where draw :: (Name, DTerm l) -> State [ArrowInfo2 l] (Diagram B, Diagram B)
            draw (name, val) = (txt name, ) <$> valDia leftSideOfStore straightShaft val
            frameStack :: [(Diagram B, Diagram B)] -> Diagram B
            frameStack stack = vcat $ map frameSlot stack
              where frameSlot (nD, vD) = boxed (1 + maxWidth fst) (h nD) nD
                                          ||| boxed (1 + maxWidth snd) (h nD) vD
                    maxWidth f = maximum (map (width . f) stack)
                    h _ = 2.5
    
    storeDia :: Map (DLocation l) (DTerm l) -> State [ArrowInfo2 l] (Diagram B)
    storeDia = fmap (vsep 0.8) . mapM draw . Map.toList
      where draw (loc, val) = named loc . framedRound <$> valDia rightSideOfStore curvedShaft val
    
    valDia :: Angle Double -> Trail V2 Double -> DTerm l -> State [ArrowInfo2 l] (Diagram B)
    -- valDia _ = treeDia
    valDia _     _     (Leaf v)    = (return . txt . show . pretty) v
    valDia _     _     (Branch [Leaf "$nat", Leaf " ", Leaf n]) = (return . txt . show . pretty) n
    valDia angle shaft (Branch xs) = hcat <$> (mapM (valDia angle shaft) xs)
    valDia angle shaft (TLoc l)    = do newId <- fmap nextId get
                                        withState ((newId, l, angle, shaft) :) (return $ point # named newId <> phm 0.5)
      where nextId [] = 0
            nextId ((i,_,_,_):_) = succ i

point :: Diagram B
point = circle 0.05 # fc black

-- "boxed" text (with dimentions)
txt :: String -> Diagram B
txt t = texterific t

phm :: Double -> Diagram B
phm s = phantom (circle s :: D V2 Double)

boxed :: Double -> Double -> Diagram B -> Diagram B
boxed w h d = d # centerXY <> rect w h

framedRound :: Diagram B -> Diagram B
framedRound d = d # centerXY <> roundedRect w h r
  where
    r = 0.9
    w = cap width
    h = cap height
    cap f = if f d < 1 then 2.5 else 2 + f d


