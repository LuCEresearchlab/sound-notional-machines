{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Diagram (
    toDiagram
  , termToTreeDiagram
  , termToTextDiagram
  , exampleSymmTree
  ) where

import Control.Monad.State.Lazy (MonadState (get), State, runState, withState)

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Tree  (Tree (Node))

import Prettyprinter (Pretty (pretty))

import Diagrams.Backend.Rasterific.CmdLine (B)
import Diagrams.Backend.Rasterific.Text    (texterific)
import Diagrams.Prelude                    (Angle, Applicative (liftA2), ArrowOpts, Diagram, IsName,
                                            Trail, TypeableFloat, V2, alignB, alignT, applyAll, arc,
                                            arrowShaft, black, centerX, centerXY, circle,
                                            composeAligned, connectOutside', connectPerim', extentX,
                                            extentY, fc, fullTurn, hcat, headGap, headLength,
                                            height, hrule, hsep, local, lw, lwO, named, normalized,
                                            opacity, pad, rad, rect, roundedRect, scale, shaftStyle,
                                            straightShaft, tau, text, turn, vcat, vsep, white,
                                            width, with, xDir, (#), (%~), (&), (.~), (@@), (|||),
                                            (~~))
import Diagrams.TwoD.Layout.Tree           (renderTree, slHSep, slHeight, slVSep, slWidth,
                                            symmLayout')

import NotionalMachines.Machine.TAPLMemoryDiagram.Main (DLocation (..), DTerm (..),
                                                        TAPLMemoryDiagram (..))

---------------
-- Tree Heap diagram
---------------

instance IsName a => IsName (DLocation a)

type Name = String

data ArrowInfo l = ArrowInfo { arrowOrigin      :: Int
                             , arrowDestination :: DLocation l
                             , arrowConnector   :: Connector l
                             }

---

type Connector l = Int -> DLocation l -> Diagram B -> Diagram B

connectToLeft, connectToRight, connectAnywhere :: IsName l => Double -> Connector l
connectToLeft   ts l1 l2 = connectPerim'   (arrowConfig ts straightShaft) l1 l2 fullTurn leftSide
connectToRight  ts l1 l2 = connectPerim'   (arrowConfig ts curvedShaft)   l1 l2 fullTurn rightSide
-- connectToTop    ts l1 l2 = connectPerim'   (arrowConfig ts curvedShaft)   l1 l2 fullTurn topSide
connectAnywhere ts       = connectOutside' (arrowConfig ts straightShaft)

leftSide, rightSide :: Angle Double
rightSide = 0     @@ rad
leftSide  = tau/2 @@ rad
-- topSide  = tau/4 @@ rad

curvedShaft :: Trail V2 Double
curvedShaft = arc xDir (1/2 @@ turn)


arrowConfig :: TypeableFloat n => n -> Trail V2 n -> ArrowOpts n
arrowConfig ts shaft = with & arrowShaft .~ shaft
                            & headLength .~ local (ts * 0.5) `atLeast` normalized 0.008
                            & headGap    .~ local (ts * 0.3)
                            & shaftStyle %~ lwO 1
          where atLeast = liftA2 max

---

toDiagram :: forall l. (IsName l) => (Double -> Connector l -> DTerm l -> State [ArrowInfo l] (Diagram B))
                                  -> Double
                                  -> TAPLMemoryDiagram l
                                  -> Diagram B
toDiagram termToDia ts taplDia = let (d, arrowLocs) = runState (allDia taplDia) []
                                  in d # applyAll (map connect arrowLocs)
  where
    connect :: ArrowInfo l -> Diagram B -> Diagram B
    connect (ArrowInfo l1 l2 connector) = connector l1 l2

    sepLine aRule size = aRule size # lwO 1 . opacity 0.2 -- . dashingL [ts * 0.03, ts * 0.03] 0
    maxim f = foldr (max . f) 0

    allDia :: TAPLMemoryDiagram l -> State [ArrowInfo l] (Diagram B)
    allDia (TAPLMemoryDiagram term nameEnv store) =
      do t <- termToDia ts (connectAnywhere ts) term
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
            draw (name, val) = (txt ts name, ) <$> termToDia ts (connectToLeft ts) val
            frameIt []   = Nothing
            frameIt rows = Just $ table (ts * 2) (ts * 2) rows
            -- frameIt rows = Just $ hsep (ts * 0.2) [ txt (ts * 0.3) "NameEnv"
            --                                       , table (ts * 2) (ts * 2) rows ]

            table :: Double -> Double -> [(Diagram B, Diagram B)] -> Diagram B
            table horizontalSpc verticalSpc rows = vcat $ map rowDia rows
              where rowDia tuple = cell fst tuple ||| cell snd tuple
                    cell col tuple = boxed (horizontalSpc + m width  (map col rows))
                                           (  verticalSpc + m height [fst tuple, snd tuple])
                                           (col tuple)
                    m dim = maximum . map dim

    storeDia :: Map (DLocation l) (DTerm l) -> State [ArrowInfo l] (Maybe (Diagram B))
    storeDia = fmap frameIt . mapM draw . Map.toList
      where draw (loc, val) = named loc . framedRound (ts * 2) 0.9 <$> termToDia ts (connectToRight ts) val
            frameIt []    = Nothing
            frameIt items = Just $ vsep ts items
            -- frameIt items = Just $ hsep (ts * 0.2) [ vsep ts items
            --                                        , txt (ts * 0.3) "Store" ]

termToTextDiagram :: Double -> Connector l -> DTerm l -> State [ArrowInfo l] (Diagram B)
termToTextDiagram ts _ (Leaf v)    = (return . txt ts . show . pretty) v
termToTextDiagram ts _ (Branch [Leaf "$nat", Leaf " ", Leaf n]) = (return . txt ts . show . pretty) n
termToTextDiagram ts c (Branch xs) = hcat <$> mapM (termToTextDiagram ts c) xs
termToTextDiagram ts c (TLoc loc)  = alignB . textCentered ts <$> locDia c loc
  where
    textCentered :: Double -> Diagram B -> Diagram B
    textCentered size d = d <> rect size (txtHeight size) # lw 0
      where
        txtHeight :: Double -> Double
        txtHeight scaleFactor = height $ txt scaleFactor "a"


-------
-------

data NodeContentElem l = Val String
                       | LLoc (DLocation l)
                       | Hole
  deriving (Eq, Show)

type NodeContent l = [NodeContentElem l]

termToTreeDiagram :: forall l. (IsName l) => Double -> Connector l -> DTerm l -> State [ArrowInfo l] (Diagram B)
termToTreeDiagram size conn = fmap (lwO 1) . renderT . termToTreeData
  where

    termToTreeData :: DTerm l -> Tree [NodeContentElem l]
    termToTreeData = \case t@(Leaf _)   -> Node [termToNodeContent t] []
                           t@(TLoc _)   -> Node [termToNodeContent t] []
                           Branch [Leaf "$nat", Leaf " ", Leaf n] -> Node [Val n] []
                           Branch terms -> Node (map termToNodeContent terms) [termToTreeData t | t@(Branch _) <- terms]
      where
        termToNodeContent :: DTerm l -> NodeContentElem l
        termToNodeContent (Leaf s)   = Val s
        termToNodeContent (TLoc l)   = LLoc l
        termToNodeContent (Branch _) = Hole

    renderT :: Tree [NodeContentElem l] -> State [ArrowInfo l] (Diagram B)
    renderT = fmap (centerXY . pad size . drawTree) . mapM drawNode
      where
        drawTree :: Tree (Diagram B) -> Diagram B
        drawTree = renderTree id (~~)
                 . symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                                     & slHeight .~ fromMaybe (0,0) . extentY)

        drawNode :: NodeContent l -> State [ArrowInfo l] (Diagram B)
        drawNode = fmap (framedRound size (size/3) . hcat) . mapM drawContentElem
          where
            drawContentElem :: NodeContentElem l -> State [ArrowInfo l] (Diagram B)
            drawContentElem (Val s) = return $ b (txt size s)
               where b v = v -- boxed (1 + width v) (1 + height v) v
            drawContentElem Hole = return $ roundedRect size size (size/3) # fc black # alignB
            drawContentElem (LLoc l) = alignB <$> locDia conn l -- circle (size/2) # fc green # alignB


-------

locDia :: Connector l -> DLocation l -> State [ArrowInfo l] (Diagram B)
locDia = locDiaState point
  where
    locDiaState :: Diagram B -> Connector l -> DLocation l -> State [ArrowInfo l] (Diagram B)
    locDiaState d conn l = do newId <- fmap nextId get
                              withState (ArrowInfo newId l conn :) (return (refOrigin newId))
      where nextId []                                = 0
            nextId (ArrowInfo { arrowOrigin = i }:_) = succ i
            refOrigin newId = d # named newId

-------

t1 :: Tree Char
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

-------
-------

-- "boxed" text (with dimentions)
txt :: Double -> String -> Diagram B
txt scaleFactor t = texterific t # scale scaleFactor

boxed :: Double -> Double -> Diagram B -> Diagram B
boxed w h d = d # centerXY <> rect w h # lwO 1

framedRound :: Double -> Double -> Diagram B -> Diagram B
framedRound spaceSize roundness d = d # centerXY <> roundedRect w h roundness # lwO 1 # fc white
  where w = spaceSize + width  d
        h = spaceSize + height d

point :: Diagram B
point = circle 0.05 # fc black

