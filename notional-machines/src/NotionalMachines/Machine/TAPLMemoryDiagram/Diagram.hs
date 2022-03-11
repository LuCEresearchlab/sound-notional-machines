{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TupleSections    #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Diagram where

-- import Data.String (fromString)

-- import Diagrams.Prelude (Diagram, (#), centerX, (===), alignT, mkWidth, centerXY, extrudeTop, rotateBy, hcat, sized)
-- import Diagrams.Backend.SVG (B, renderSVG)
-- import Diagrams.SVG.ReadSVG (readSVGLBS)

-- import           Data.Map (Map)
-- import qualified Data.Map as Map

-- import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamilyF (..), Color, colorHexa)
-- import NotionalMachines.Utils (replace, diaSeq)
-- import NotionalMachines.Machine.TAPLMemoryDiagram.Main (TAPLMemoryDiagram, Name)

import Control.Monad.State.Lazy


import Data.Typeable (Typeable)

import Diagrams.Prelude hiding (Name, Loc)
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific.Text

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
            frameStack stack = vcat $ map frameSlot stack
              where frameSlot (nameD, valD) = boxed (1 + maxWidth fst) (h nameD) nameD
                                          ||| boxed (1 + maxWidth snd) (h nameD) valD
                    maxWidth f = maximum (map (width . f) stack)
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

-------------

-- toDiagram :: Double -> TAPLMemoryDiagram -> Diagram B
-- toDiagram size md = hsep 2 ds # applyAll (map (uncurry connectOutside) arrowLocs)
--   where
--     (ds, arrowLocs) = runState (sequence [nameEnvDia (taplNameEnv md), storeDia (taplStore md)]) []

--     nameEnvDia :: Map Name t -> State [(Int, Location)] (Diagram B) 
--     nameEnvDia = fmap vcat . mapM draw
--       where draw (name, val) = slotDia name <$> valDia val
--             slotDia t d = (text t <> rect 4 (height d)) ||| d # centerXY # framed
    
--     storeDia :: Map (Location l) t -> State [(Int, Location)] (Diagram B)
--     storeDia = fmap (vsep 0.8) . mapM draw
--       where draw (loc, val) = named loc . framedRound <$> valDia val
    
--     valDia :: Val -> State [(Int, Location)] (Diagram B)
--     -- valDia _ = treeDia
--     valDia (Val v) = return $ txt 1 (show v)
--     valDia (Loc v) = do newId <- fmap nextId get
--                         withState ((newId, v) :) (return $ point # named newId <> phm 1)
--       where nextId [] = 0
--             nextId ((i,_):_) = succ i
    
point = circle 0.05 # fc black

-- "boxed" text (with dimentions)
txt :: String -> Diagram B
txt t = texterific t

phm s = phantom (circle s :: D V2 Double)

boxed w h d = d # centerXY <> rect w h

framedRound d = d # centerXY <> roundedRect w h r
  where
    r = 0.9
    w = cap width
    h = cap height
    cap f = if f d < 1 then 2.5 else 2 + f d


--------------------------
--------------------------

-- mainStackAndHeap :: Diagram B
-- mainStackAndHeap = bgFrame 1 white $ stackAndHeap stackEx heapEx
