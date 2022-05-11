{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Diagram (
    toDiagram
  , termToTreeDiagram
  , termToTextDiagram
  , exampleSymmTree
  ) where
import Control.Monad.State.Lazy ( runState, withState, MonadState(get), State )

import           Data.Map (Map)
import qualified Data.Map as Map

import Prettyprinter (Pretty (pretty))

import Diagrams.Prelude (Diagram, IsName, _arrowHead, _arrowShaft, _headLength, (#), centerXY, rect, roundedRect, width, height, circle, hcat, named, fc, black, vsep, (|||), (@@), straightShaft, rad, arc, tau, xDir, turn, applyAll, connectPerim', fullTurn, vcat, scale, normalized, local, centerX, Applicative (liftA2), connectOutside', alignB, lw, Angle, V2, Trail, lwO, with, (&), (.~), arrowShaft, headLength, headGap, shaftStyle, (%~), hsep, hrule, alignT, composeAligned, opacity, text, white, (~~), pad, extentY, green)
import Diagrams.Backend.Rasterific.CmdLine ( B )
import Diagrams.Backend.Rasterific.Text ( texterific )

import NotionalMachines.Machine.TAPLMemoryDiagram.Main (TAPLMemoryDiagram(..), DTerm(..), DLocation(..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Tree (Tree (Node))
import Diagrams.TwoD.Layout.Tree (renderTree, symmLayout', slHSep, slVSep, slHeight, slWidth)
import Diagrams.TwoD (extentX)

---------------
-- Tree Heap diagram
---------------

type Name = String

data ArrowInfo l = ArrowInfo { arrowOrigin :: Int, arrowDestination :: DLocation l, arrowConnector :: Connector l }
type Connector l = Int -> DLocation l -> Diagram B -> Diagram B

instance IsName a => IsName (DLocation a)

toDiagram :: forall l. (IsName l) => (Double -> Connector l -> DTerm l -> State [ArrowInfo l] (Diagram B))
                                  -> Double
                                  -> TAPLMemoryDiagram l
                                  -> Diagram B
toDiagram termToDia ts taplDia = let (d, arrowLocs) = runState (allDia taplDia) []
                                  in d # applyAll (map connect arrowLocs)
  where
    connectToLeft  l1 l2 = connectPerim'   (arrowConfig straightShaft) l1 l2 fullTurn leftSide
    connectToRight l1 l2 = connectPerim'   (arrowConfig curvedShaft)   l1 l2 fullTurn rightSide
    connectAnywhere      = connectOutside' (arrowConfig straightShaft)

    arrowConfig shaft = with & arrowShaft .~ shaft
                             & headLength .~ local (ts * 0.5) `atLeast` normalized 0.008
                             & headGap    .~ local (ts * 0.3)
                             & shaftStyle %~ lwO 1
              where atLeast = liftA2 max

    connect :: ArrowInfo l -> Diagram B -> Diagram B
    connect (ArrowInfo l1 l2 connector) = connector l1 l2

    sepLine aRule size = aRule size # lwO 1 . opacity 0.2 -- . dashingL [ts * 0.03, ts * 0.03] 0
    maxim f = foldr (max . f) 0

    allDia :: TAPLMemoryDiagram l -> State [ArrowInfo l] (Diagram B)
    allDia (TAPLMemoryDiagram term nameEnv store) =
      do t <- termToDia ts connectAnywhere term
         env <- nameEnvDia nameEnv
         s <- storeDia store
         let memDiaParts = catMaybes [env, s]
        --  let vline = sepLine vrule (maxim height memDiaParts)
         let memDia = (hsep (ts * 2)  # composeAligned alignT) memDiaParts # centerX --(intersperse vline memDiaParts) # centerX
         let hline = sepLine hrule (maxim width [t, memDia])
         return $ vsep ts (t # centerX : if null memDiaParts then [] else [hline, memDia])
        --  return $ vsep (ts * 0.1) (intersperse hline [t # centerX, memDia])

    nameEnvDia :: Map Name (DTerm l) -> State [ArrowInfo l] (Maybe (Diagram B))
    nameEnvDia = fmap frameIt . mapM draw . Map.toList
      where draw :: (Name, DTerm l) -> State [ArrowInfo l] (Diagram B, Diagram B)
            draw (name, val) = (txt ts name, ) <$> termToDia ts connectToLeft val
            frameIt []   = Nothing
            frameIt rows = Just $ table (ts * 2) (ts * 2) rows
            -- frameIt rows = Just $ hsep (ts * 0.2) [ txt (ts * 0.3) "NameEnv"
            --                                       , table (ts * 2) (ts * 2) rows ]

    storeDia :: Map (DLocation l) (DTerm l) -> State [ArrowInfo l] (Maybe (Diagram B))
    storeDia = fmap frameIt . mapM draw . Map.toList
      where draw (loc, val) = named loc . framedRound (ts * 2) 0.9 <$> termToDia ts connectToRight val
            frameIt []    = Nothing
            frameIt items = Just $ vsep ts items
            -- frameIt items = Just $ hsep (ts * 0.2) [ vsep ts items
            --                                        , txt (ts * 0.3) "Store" ]

termToTextDiagram :: Double -> Connector l -> DTerm l -> State [ArrowInfo l] (Diagram B)
termToTextDiagram ts _ (Leaf v)    = return $ (txt ts . show . pretty) v
termToTextDiagram ts _ (Branch [Leaf "$nat", Leaf " ", Leaf n]) = return $ (txt ts . show . pretty) n
termToTextDiagram ts c (Branch xs) = hcat <$> mapM (termToTextDiagram ts c) xs
termToTextDiagram ts c (TLoc l)    = alignB . textCentered ts <$> locDia point c l

locDia :: Diagram B -> Connector l -> DLocation l -> State [ArrowInfo l] (Diagram B)
locDia d c l = do newId <- fmap nextId get
                  withState (ArrowInfo newId l c :) (return (refOrigin newId))
  where nextId [] = 0
        nextId (ArrowInfo { arrowOrigin = i }:_) = succ i
        refOrigin newId = d # named newId

point :: Diagram B
point = circle 0.05 # fc black

textCentered :: Double -> Diagram B -> Diagram B
textCentered size d = d <> rect size (txtHeight size) # lw 0

-- "boxed" text (with dimentions)
txt :: Double -> String -> Diagram B
txt scaleFactor t = texterific t # scale scaleFactor

txtHeight :: Double -> Double
txtHeight scaleFactor = height $ txt scaleFactor "a"

boxed :: Double -> Double -> Diagram B -> Diagram B
boxed w h d = d # centerXY <> rect w h # lwO 1

framedRound :: Double -> Double -> Diagram B -> Diagram B
framedRound spaceSize roundness d = d # centerXY <> roundedRect w h roundness # lwO 1 # fc white
  where w = spaceSize + width  d
        h = spaceSize + height d

table :: Double -> Double -> [(Diagram B, Diagram B)] -> Diagram B
table horizontalSpc verticalSpc rows = vcat $ map rowDia rows
  where rowDia tuple = cell fst tuple ||| cell snd tuple
        cell col tuple = boxed (horizontalSpc + m width  (map col rows))
                               (  verticalSpc + m height [fst tuple, snd tuple])
                               (col tuple)
        m dim = maximum . map dim

leftSide, rightSide :: Angle Double
rightSide = 0     @@ rad
leftSide  = tau/2 @@ rad

curvedShaft :: Trail V2 Double
curvedShaft = arc xDir (1/2 @@ turn)


-------
-------

data NodeContent l = Val String
                   | LLoc (DLocation l)
                   | Hole
                   deriving (Eq, Show)

treeToTree :: DTerm l -> Tree [NodeContent l]
treeToTree = \case t@(Leaf _)   -> Node [termToNodeContent t] []
                   t@(TLoc _)   -> Node [termToNodeContent t] []
                   Branch [Leaf "$nat", Leaf " ", Leaf n] -> Node [Val n] []
                   Branch terms -> Node (map termToNodeContent terms) [treeToTree t | t@(Branch _) <- terms]
  where
    termToNodeContent :: DTerm l -> NodeContent l
    termToNodeContent (Leaf s)   = Val s
    termToNodeContent (TLoc l)   = LLoc l
    termToNodeContent (Branch _) = Hole

renderT :: Double -> Tree [NodeContent l] -> Diagram B
renderT size = centerXY
             . pad size
             . drawTree
             . fmap drawType

  where
    drawTree :: Tree (Diagram B) -> Diagram B
    drawTree = renderTree id (~~)
             . symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                                 & slHeight .~ fromMaybe (0,0) . extentY) 

    drawType :: [NodeContent l] -> Diagram B
    drawType = framedRound size (size/3). hcat . map drawNode

    drawNode :: NodeContent l -> Diagram B
    drawNode (Val s) = b (txt size s)
       where b v = v -- boxed (1 + width v) (1 + height v) v
    drawNode Hole = roundedRect size size (size/3) # fc black # alignB
    drawNode (LLoc _) = circle (size/2) # fc green # alignB

termToTreeDiagram :: Double -> Connector l -> DTerm l -> State [ArrowInfo l] (Diagram B)
termToTreeDiagram size _ = return . lwO 1 . renderT size . treeToTree

-------

t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJKLM"), Node 'N' (map lf "OPQR")]]
  where lf x = Node x []

exampleSymmTree :: Diagram B
exampleSymmTree =
  renderTree ((<> circle 1) . text . (:[]))
             (~~)
             (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t1)
  # centerXY # pad 1.1

-------

--    :renderTrace if iszero (pred 2) then 0 else (if iszero (pred 0) then succ 2 else 0) -->* 3 : Nat:
--    :renderTrace (\x:Nat. ((\z:Nat. (\zz:Nat->Nat. zz)) x) ((\x:Bool. (\w:Bool. (\y:Nat. y)) x) true) x) 1 -->* 1 : Nat:
--    :renderTrace (\zz:Nat->Nat. (\xx:Nat->Nat->Nat. xx 1 2) (\y:Nat. zz)) (\z:Nat. z) -->* 2 : Nat:
--    :renderTrace (\r:Ref Nat. if false then (r := 82; !r) else (!r)) (ref 13) -->* 13 : Nat:
--    :renderTrace (\r:Ref Nat. r:=succ(!r); r:=succ(!r); !r) (ref 0) -->* 2 : Nat:
--    :renderTrace (\r:Ref Nat.(\s:Ref Nat.          !r) r) (ref 13) -->* 13 : Nat:
--    :renderTrace (\r:Ref Nat.(\s:Ref Nat. s := 82; !r) r) (ref 13) -->* 82 : Nat:
--    :renderTrace (\r:Ref Nat.(\s:Ref Nat. r := 0; r := !s; !r)) (ref 2) (ref 2) -->* 2 : Nat:
--    :renderTrace (\x:Ref Nat.(\r:Ref Nat.(\s:Ref Nat.r := 0; r := !s; !r)) x x) (ref 2) -->* 0 : Nat:
--    :renderTrace (\x:Ref Nat.(\r:Ref Nat.(\s:Ref Nat.r := 0; r := !s; !r)) x x) (ref 2) -->* 0 : Nat:
-- LambdaRef ala Wadler
--    :renderTrace (\x:Nat. ((\z:Nat. (\zz:Nat->Nat. zz)) x) ((\x:Bool. (\w:Bool. (\y:Nat. y)) x) true) x) 1 -->* 1 : Nat:
--    :renderTrace (\zz:Nat->Nat. (\xx:Nat->Nat->Nat. xx 1 2) (\y:Nat. zz)) (\z:Nat. z) -->* 2 : Nat:
-- LambdaRef ala Racket
--    :renderTrace (\x:Nat. ((\z:Nat. (\zz:Nat->Nat. zz)) x) ((\x:Bool. (\w:Bool. (\y:Nat. y)) x) true) x) 1 -->* 1 : Nat:
--    :renderTrace (\zz:Nat->Nat. (\xx:Nat->Nat->Nat. xx 1 2) (\y:Nat. zz)) (\z:Nat. z) -->* 2 : Nat:
--    :renderTrace (\x:Nat->Nat. (\x:Nat->Nat->Nat. (\z:Nat->Nat->Nat->Nat. z 9) (\w:Nat. x) 0 1) (\y:Nat. x)) (\x:Nat. x) -->* 1 : Nat:
--    :renderTrace ref (ref 1) -->* Loc 1 : Ref (Ref Nat):

--    :renderTrace (\r:Nat->Ref (Nat->Nat).(\s:Ref Nat. s := 82; !(r 13)) (r 13)) (\x:Nat. ref if iszero (\x:Nat. succ x)) -->* 82 : Nat:
--    :renderTrace (\timer:Unit->Unit. (\getVal:Unit->Nat. timer unit; timer unit; timer unit; getVal unit)) (timerMaker state)
--                        (\timerMaker: . (timerMaker 13)) (\init:Nat. (\c:Ref Nat. (\x:Unit. c != succ (!c))) (ref init))

