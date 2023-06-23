{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NotionalMachines.LangInMachine.UntypedLambdaExpressionTree (
      bisim
    , nmToLang

    , repl

    , ascii
    , asciiTrace
    , diagram
    , diagramTrace
    ) where

import Diagrams.Backend.Rasterific (renderRasterific)
import Diagrams.Prelude            (Diagram)

import NotionalMachines.Lang.Error              (Error)
import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse)

import           NotionalMachines.Machine.ExpressionTree.BoxesDiagram  as Boxes (toDiagram)
import qualified NotionalMachines.Machine.ExpressionTree.BubbleDiagram as Bubble (toDiagram)
import           NotionalMachines.Machine.ExpressionTree.Main          (ExpAsTree (..))

import NotionalMachines.Meta.Bijective    (Bijective, fromNM)
import NotionalMachines.Meta.Bisimulation (Bisimulation, mkBijBisim, mkStepBijNM)
import NotionalMachines.Meta.LangToNM     (LangToNM (..))
import NotionalMachines.Meta.Steppable    (Steppable, eval, step, trace)

import NotionalMachines.Util.Diagrams (renderD, vDiaSeq)
import NotionalMachines.Util.REPL     (LangPipeline (..), mkCmd, mkLangReplOpts)

langToNM :: Exp -> ExpAsTree
langToNM (Var name)      = Box name
langToNM (Lambda name e) = LambdaBox name (langToNM e)
langToNM (App e1 e2)     = BinaryBox (langToNM e1) (langToNM e2)

nmToLang :: ExpAsTree -> Exp
nmToLang (Box name)         = Var name
nmToLang (LambdaBox name e) = Lambda name (nmToLang e)
nmToLang (BinaryBox e1 e2)  = App (nmToLang e1) (nmToLang e2)

instance LangToNM Exp ExpAsTree where
  toNM = langToNM

instance Bijective Exp ExpAsTree where
  fromNM = nmToLang

instance Steppable ExpAsTree where
  step = mkStepBijNM step

bisim :: Bisimulation Exp Exp ExpAsTree ExpAsTree
bisim = mkBijBisim
-- bisim = Bisim { fLang  = eval
--               , fNM    = toNM . eval . fromNM
--               , alphaA = toNM
--               , alphaB = toNM }

-- -- type ANM = ExpAsTree
-- -- type BNM = ExpAsTree
-- -- alphaA :: APL -> ANM
-- -- alphaA = langToNM
-- -- alphaA' :: ANM -> APL
-- -- alphaA' = NMToLang
-- -- alphaB :: BPL -> BNM
-- -- alphaB = alphaA
-- -- fNM :: ANM -> BNM
-- -- fNM = alphaB . fPL . alphaA'



langPipeline :: LangPipeline Exp () Error [Exp]
langPipeline = LangPipeline parse (Right . eval) Nothing (Right . trace)

-- TODO: Reduce code duplication between this and the other repls
repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("ascii",       ascii)
    , ("asciiTrace",  asciiTrace)
    , ("renderBubble",      r . diagram Bubble.toDiagram)
    , ("renderBubbleTrace", r . traceDiagram . diagramTrace Bubble.toDiagram)
    , ("renderBoxes",       r . diagram Bubble.toDiagram)
    , ("renderBoxesTrace",  r . traceDiagram . diagramTrace Bubble.toDiagram)
    ] "ExpressionTree>" helpMsg langPipeline
  where helpMsg = "Play with the Expression as Tree notional machine for Untyped Lambda Calculus"
        r = renderD renderRasterific fileName w

ascii :: String -> IO ()
ascii =       mkCmd . fmap (: []) . str2NM

asciiTrace :: String -> IO ()
asciiTrace =  mkCmd . fmap trace  . str2NM

diagram :: (ExpAsTree -> IO (Diagram b)) -> String -> IO (Either Error (Diagram b))
diagram toDia = mapM toDia . str2NM

diagramTrace :: (ExpAsTree -> IO (Diagram b)) -> String -> IO (Either Error [Diagram b])
diagramTrace toDia = mapM (mapM toDia) . fmap trace . str2NM

traceDiagram :: _ => IO (Either Error [Diagram b]) -> IO (Either Error (Diagram b))
traceDiagram = fmap (fmap (vDiaSeq 1.5 0.5))

str2NM :: String -> Either Error ExpAsTree
str2NM = fmap langToNM . parse

