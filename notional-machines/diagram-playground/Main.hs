{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Diagrams.Prelude hiding (Color, trace)
import Diagrams.Backend.SVG.CmdLine

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse, unparse)
import NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs (diagramTrace, traceDiagram, diagram, str2NM, langToNm)
import NotionalMachines.Meta.Steppable (Steppable (trace), allPoints, step)
import NotionalMachines.Util.Diagrams (diagramWithError, text', tightText, fontMono, text'', connectOutside'')
import NotionalMachines.Examples.Diagrams (langAndNMTrace)
import NotionalMachines.Machine.AlligatorEggs.Diagram
    ( toDiagram, egg, oldAlligator, hungryAlligator )
import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color(MkColor, MkColorFromName))
import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamilyF(OldAlligator, HungryAlligator, Egg), evolve, wrongEvolve)
import Diagrams.SVG.Path (PathCommand(H))
import NotionalMachines.Lang.Error (Error)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

alligatorRenderTrace = traceDiagram . diagramTrace

t = langAndNMTrace 0.05 diagramTrace (fmap trace . parse)

dia :: _ => IO (Diagram B)
dia = (diagramWithError . alligatorRenderTrace)
      "(λt. λf. t) (\\a.a) (\\b.b)"
      --"(\\t. \\f. t) a b"

pieces :: _ => IO (Diagram B)
pieces = do
    let t = frame 0.5 . centerXY . sized (mkHeight 1)
    [h, e, o] <- fmap (fmap t) . sequence $
                 [hungryAlligator "green", egg "gray", oldAlligator]
    return $ hcat [vcat [txt "Hungry Alligator", h]
                  ,vcat [txt "Egg", e]
                  ,vcat [txt "Old Alligator", o]]
    where txt s = text' black 0.4 s # centerXY


family1 :: _ => IO (Diagram B)
family1 = (diagramWithError . diagram)
          "(λt. λf. t f)"

family2 :: _ => IO (Diagram B)
family2 = (diagramWithError . diagram)
          "(λt. λf. \\x. t (t f) x)"

eatingRule :: _ => IO (Diagram B)
eatingRule = (diagramWithError . alligatorRenderTrace)
             "(λt. λf. t) (\\a.a)"

recoloringRule :: _ => IO (Diagram B)
recoloringRule = (diagramWithError . alligatorRenderTrace)
                 "(λt. λf. t) (\\t.t)"

oldAlligatorRule :: _ => IO (Diagram B)
oldAlligatorRule = diagramWithError . traceDiagram . fmap return . mapM toDiagram . trace $ family
    where v1 = MkColorFromName "t"
          v2 = MkColorFromName "f"
          family = [OldAlligator
                        [HungryAlligator v1 [Egg v1],
                         HungryAlligator v2 [Egg v2]]]

correctRecoloringRule :: IO (Diagram B)
correctRecoloringRule = (diagramWithError . alligatorRenderTrace)
                 "(λt. (λt. t) (λf. t)) a"

wrongAlligatorRenderTrace :: String -> IO (Either Error (Diagram B))
wrongAlligatorRenderTrace = traceDiagram . diagramTrace
    where diagramTrace = mapM (mapM toDiagram) . fmap trace . str2NM
          trace = allPoints wrongEvolve

wrongRecoloringRule :: IO (Diagram B)
wrongRecoloringRule = do d <- (diagramWithError . wrongAlligatorRenderTrace) "(λt. (λt. t) (λf. t)) a"
                         return $ d <> boundingRect d # lwO 1

foo :: _ => IO (Diagram B)
foo = t "(λt. λf. t f){a}"
    where t s = do font <- fontMono
                   return $ text'' font black 1 s # sized (mkWidth 1)

bar :: _ => IO (Diagram B)
bar = return $ text "hi" <> circle 1
--bar = return $ t "(λt. λf. t f){a}" <> circle 1
    where t s = text' black 1 s # sized (mkWidth 1)




commutativeDiagram :: Diagram B
commutativeDiagram = vertices
        # c "A_PL" "A_NM" 0.15 (label "alpha")
        # c "A_NM" "B_NM" 0.10 (label "f_NM")
        # c "A_PL" "B_PL" 0.10 (label "f_PL")
        # c "B_PL" "B_NM" 0.15 (label "alpha")
  where c = connectOutside'' (with & gaps .~ small
                                   & headLength .~ local 0.05)

        label s = text s # fontSize (local 0.1)
               <> rect 0.3 0.12 # lw none

        vertices :: Diagram B
        vertices = atPoints (trailVertices $ rect 1.5 1)
                            [t "B_PL", t "B_NM", t "A_NM", t "A_PL"]
          where t s = label s
                    # named s
                    # centerXY

wrongAlligatorDiagram :: IO (Diagram B)
wrongAlligatorDiagram = do
        font <- fontMono
        let e2 = step e1
        a1 <- alligator . langToNm $ e1
        a3 <- alligator . wrongEvolve . langToNm $ e1
        a2 <- alligator . langToNm $ e2
        return $ vertices [t font (unparse e2) # named "B_PL",
                           a2 # named "B_NM1",
                           a3 # named "B_NM2",
                           a1 # named "A_NM",
                           t font (unparse e1) # named "A_PL"]
               # c "A_PL" "A_NM" 0.15 (label "alpha")
               # c "A_NM" "B_NM2" 0.10 (label "f_NM")
               # c "A_PL" "B_PL" 0.10 (label "f_PL")
               # c "B_PL" "B_NM1" 0.15 (label "alpha")
  where c = connectOutside'' (def { _headGap = small,
                                    _tailGap = small,
                                    _shaftStyle = mempty # lw thin,
                                    _headLength = local 0.05})

        label s = text s # fontSize (local 0.1)
               <> rect 0.3 0.12 -- # lw none

        t font s = d <> rect (width d) (height d) # lw none
            where d = text'' font black 0.1 s # centerXY
        alligator fs = fmap (centerXY . sized (mkWidth (0.3 * fromIntegral (length fs))))
                     . toDiagram $ fs

        vertices = atPoints [p2 (w,0), p2 (w,m*h), p2 (m*w,h), p2 (0,h), p2 (0,0)]
            where w = 1.5
                  h = 1
                  m = 0.7

        e1 = App (Lambda "t" (Lambda "t" (Var "t"))) (Lambda "a" (Var "a"))


main = mainWith . prep =<< wrongAlligatorDiagram
    where prep = bgFrame 0.05 white



--    Alligators
--      soundness condition:                                                                                      FAIL (0.13s)
--          ✗ <interactive> failed at test/Spec.hs:139:7
--            after 94 tests and 11 shrinks.
--          
--                ┏━━ test/Spec.hs ━━━
--            136 ┃ isEquivalentTo :: (Eq a, Show a, Show e) => Gen e -> (e -> a) -> (e -> a) -> Property
--            137 ┃ isEquivalentTo g f f' = prop $ do
--            138 ┃   e <- forAll g
--                ┃   │ App
--                ┃   │   (Lambda
--                ┃   │      "a"
--                ┃   │      (App
--                ┃   │         (Lambda
--                ┃   │            "a"
--                ┃   │            (App
--                ┃   │               (Lambda "b" (Var "a")) (Lambda "a" (App (Var "a") (Var "a")))))
--                ┃   │         (Lambda "a" (Var "a"))))
--                ┃   │   (Lambda "a" (Var "a"))
--            139 ┃   f e === f' e
--                ┃   ^^^^^^^^^^^^
--                ┃   │ ━━━ Exception (ErrorCall) ━━━
--                ┃   │ Prelude.!!: negative index
--          
--            This failure can be reproduced by running:
--            > recheck (Size 93) (Seed 15231851506382136837 8747451747690855601) <property>
--          
--        Use '--pattern "$NF ~ /soundness condition/" --hedgehog-replay "Size 93 Seed 15231851506382136837 8747451747690855601"' to reproduce from the command-line.
--        
--        Use -p '/Alligators.soundness condition/' to rerun this test only.
--