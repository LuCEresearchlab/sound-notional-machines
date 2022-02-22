{-# OPTIONS_GHC -Wall #-}

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
-- import Control.Monad.State.Lazy (State)

-- data Val = Val Int | Loc Loc

-- newtype Loc = MkLoc Int
--   deriving (Typeable, Eq, Ord, Show)
-- instance IsName Loc

-- type Name = String

-- type Stack = [(Name, Val)]
-- stackEx :: Stack
-- stackEx = zip (map return ['a'..]) [Val 1, Loc (MkLoc 4), Val 5, Loc (MkLoc 3), Val 10, Val 2]

-- type Heap = [(Loc, Val)]
-- heapEx :: Heap
-- heapEx = zip (map MkLoc [0..]) [Val 5, Val 6, Val 7, Loc (MkLoc 2), Val 9]

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
    
--     point = circle 0.05 # fc black
    
--     txt :: Double -> String -> Diagram B
--     txt s t = text t # fontSizeL s <> phm s
    
--     phm s = phantom (circle s :: D V2 Double)