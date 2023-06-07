{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module NotionalMachines.Machine.TAPLMemoryDiagram.Diagram (
    toDiagram'
  , toDiagram
  , toDiagramSeq

  , TermToDiagram
  , termToTreeDiagram
  , termToTextDiagram

  , exampleSymmTree
  ) where

import Control.Monad.State.Lazy (MonadState (get), State, runState, withState)

import qualified Data.Map   as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Tree  (Tree (Node))

import Prettyprinter (Pretty (pretty))

import Diagrams.Prelude          (Angle, Any, Applicative (liftA2), ArrowOpts, D, Diagram, IsName,
                                  QDiagram, Trail, TypeableFloat, V2, alignB, alignT, applyAll, arc,
                                  arrowShaft, bgFrame, black, centerX, centerXY, circle,
                                  composeAligned, connectOutside', connectPerim', extentX, extentY,
                                  fc, fullTurn, hcat, headGap, headLength, height, hrule, hsep,
                                  local, lw, lwO, named, normalized, opacity, pad, phantom, rad,
                                  rect, roundedRect, shaftStyle, straightShaft, tau, text, turn,
                                  vcat, vsep, white, width, with, xDir, (#), (%~), (&), (.~), (@@),
                                  (|||), (~~))
import Diagrams.TwoD.Layout.Tree (renderTree, slHSep, slHeight, slVSep, slWidth, symmLayout')

import NotionalMachines.Machine.TAPLMemoryDiagram.Main (DLocation (..), DTerm (..),
                                                        TAPLMemoryDiagram (..))

import NotionalMachines.Util.Diagrams (vDiaSeq)

---------------
-- Tree Heap diagram
---------------

toDiagram    :: _ => TermToDiagram l b -> TAPLMemoryDiagram l -> IO (QDiagram b V2 Double Any)
toDiagram f = return . toDiagram' f 1

toDiagramSeq :: _ => TermToDiagram l b -> [TAPLMemoryDiagram l] -> IO (QDiagram b V2 Double Any)
toDiagramSeq f = fmap (vDiaSeq 1.5 0.5) . mapM (toDiagram f)



instance IsName a => IsName (DLocation a)

data ArrowInfo l b = ArrowInfo { arrowOrigin      :: Int
                               , arrowDestination :: DLocation l
                               , arrowConnector   :: Connector l b
                               }

---

type Connector l b = Int -> DLocation l -> Diagram b -> Diagram b

connectToLeft, connectToRight, connectAnywhere :: _ => IsName l => Double -> Connector l b
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

bg :: _ => QDiagram b V2 Double Any -> QDiagram b V2 Double Any
bg = bgFrame 0.05 white

toDiagram' :: (IsName l, _) => TermToDiagram l b
                            -> Double
                            -> TAPLMemoryDiagram l
                            -> QDiagram b V2 Double Any
toDiagram' termToDia ts taplDia = let (d, arrowLocs) = runState (allDia taplDia) []
                                  in d # applyAll (map connect arrowLocs) # bg
  where
    connect :: ArrowInfo l _ -> Diagram _ -> Diagram _
    connect (ArrowInfo l1 l2 connector) = connector l1 l2

    sepLine aRule size = aRule size # lwO 1 . opacity 0.2 -- . dashingL [ts * 0.03, ts * 0.03] 0
    maxim f = foldr (max . f) 0

    -- allDia :: TAPLMemoryDiagram l -> State [ArrowInfo l _] (Diagram _)
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

    -- nameEnvDia :: Map Name (DTerm l) -> State [ArrowInfo l _] (Maybe (Diagram _))
    nameEnvDia = fmap frameIt . mapM draw . Map.toList
      where
            -- draw :: (Name, DTerm l) -> State [ArrowInfo l _] (Diagram _, Diagram _)
            draw (name, val) = (txt ts name, ) <$> termToDia ts (connectToLeft ts) val
            frameIt []   = Nothing
            frameIt rows = Just $ table (ts * 2) (ts * 2) rows
            -- frameIt rows = Just $ hsep (ts * 0.2) [ txt (ts * 0.3) "NameEnv"
            --                                       , table (ts * 2) (ts * 2) rows ]

            -- table :: Double -> Double -> [(Diagram _, Diagram _)] -> Diagram _
            table horizontalSpc verticalSpc rows = vcat $ map rowDia rows
              where rowDia tuple = cell fst tuple ||| cell snd tuple
                    cell col tuple = boxed (horizontalSpc + m width  (map col rows))
                                           (  verticalSpc + m height [fst tuple, snd tuple])
                                           (col tuple)
                    m dim = maximum . map dim

    -- storeDia :: _ => Map (DLocation l) (DTerm l) -> State [ArrowInfo l _] (Maybe (QDiagram b V2 Double Any))
    storeDia = fmap frameIt . mapM draw . Map.toList
      where draw (loc, val) = named loc . framedRound (ts * 2) 0.9 <$> termToDia ts (connectToRight ts) val
            frameIt []    = Nothing
            frameIt items = Just $ vsep ts items
            -- frameIt items = Just $ hsep (ts * 0.2) [ vsep ts items
            --                                        , txt (ts * 0.3) "Store" ]


type TermToDiagram l b = Double
                      -> Connector l b
                      -> DTerm l
                      -> State [ArrowInfo l b] (QDiagram b V2 Double Any)

termToTextDiagram :: _ => TermToDiagram l b
termToTextDiagram ts _ (Leaf v)    = (return . txt ts . show . pretty) v
termToTextDiagram ts c (Branch xs) = hcat <$> mapM (termToTextDiagram ts c) xs
termToTextDiagram ts c (TLoc loc)  = alignB . textCentered ts <$> locDia c loc

-------
-------

data NodeContentElem l = Val String
                       | LLoc (DLocation l)
                       | Hole
  deriving (Eq, Show)

termToTreeDiagram :: _ => TermToDiagram l b
termToTreeDiagram size conn = fmap (lwO 1) . renderT . termToTreeData
  where

    termToTreeData :: DTerm l -> Tree [NodeContentElem l]
    termToTreeData = \case t@(Leaf _)   -> Node [termToNodeContent t] []
                           t@(TLoc _)   -> Node [termToNodeContent t] []
                           Branch terms -> Node (map termToNodeContent terms) [termToTreeData t | t@(Branch _) <- terms]
      where
        termToNodeContent :: DTerm l -> NodeContentElem l
        termToNodeContent (Leaf s)   = Val s
        termToNodeContent (TLoc l)   = LLoc l
        termToNodeContent (Branch _) = Hole

    renderT = fmap (centerXY . pad size . drawTree) . mapM drawNode
      where
        drawTree = renderTree id (~~)
                 . symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                                     & slHeight .~ fromMaybe (0,0) . extentY)

        drawNode = fmap (framedRound size (size/3) . hcat) . mapM drawContentElem
          where
            drawContentElem (Val s) = return $ b (txt size s)
               where b v = v -- boxed (1 + width v) (1 + height v) v
            drawContentElem Hole = return $ roundedRect size size (size/3) # fc black # alignB
            drawContentElem (LLoc l) = alignB <$> locDia conn l -- circle (size/2) # fc green # alignB


-------

locDia :: _ => Connector l _ -> DLocation l -> State [ArrowInfo l _] (QDiagram b V2 Double Any)
locDia = locDiaState point
  where
    locDiaState :: Diagram _ -> Connector l _ -> DLocation l -> State [ArrowInfo l _] (Diagram _)
    locDiaState d conn l = do newId <- fmap nextId get
                              withState (ArrowInfo newId l conn :) (return (refOrigin newId))
      where nextId []                                = 0
            nextId (ArrowInfo { arrowOrigin = i }:_) = succ i
            refOrigin newId = d # named newId

-------

t1 :: Tree Char
t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJKLM"), Node 'N' (map lf "OPQR")]]
  where lf x = Node x []

exampleSymmTree :: _ => QDiagram b V2 Double Any
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
txt :: _ => Double -> String -> QDiagram b V2 Double Any
-- txt scaleFactor t = text t <> rect w h
txt scaleFactor t = text t <> phantom (rect w h :: D V2 Double)
  where w = scaleFactor * fromIntegral (length t)
        h = txtHeight scaleFactor

textCentered :: _ => Double -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
textCentered size d = d <> rect size (txtHeight size) # lw 0

txtHeight :: Double -> Double
txtHeight scaleFactor = 1.2 * scaleFactor

boxed :: _ => Double -> Double -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
boxed w h d = d # centerXY <> rect w h # lwO 1

framedRound :: _ => Double -> Double -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
framedRound spaceSize roundness d = d # centerXY <> roundedRect w h roundness # lwO 1 # fc white
  where w = spaceSize + width  d
        h = spaceSize + height d

point :: _ => QDiagram b V2 Double Any
point = circle 0.05 # fc black

