{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.UntypedLambdaExpressionTree (
      bisim
    , langToNM
    , nmToLang

    , repl
    , ascii
    , asciiTrace
    , render
    , renderTrace
    ) where

import Prettyprinter (Pretty (pretty), vsep)
import Text.Parsec   (ParseError)

import Diagrams.Backend.Rasterific (B)
import Diagrams.Prelude            (Diagram, bgFrame, white)

import NotionalMachines.Lang.UntypedLambda.Main        (Exp (..), parse)
import NotionalMachines.Machine.ExpressionTree.Diagram (toDiagram)
import NotionalMachines.Machine.ExpressionTree.Main    (ExpAsTree (..), toAscii)

import NotionalMachines.Meta.Bijective    (Bijective, fromNM, toNM)
import NotionalMachines.Meta.Bisimulation (Bisimulation, mkBijBisim, stepNM)
import NotionalMachines.Meta.Steppable    (Steppable, eval, step, trace)

import NotionalMachines.Util.Diagrams (renderDiagramRaster, vDiaSeq)
import NotionalMachines.Util.REPL     (LangPipeline (..), mkCmd, mkLangReplOpts)

langToNM :: Exp -> ExpAsTree
langToNM (Var name)      = Box name
langToNM (Lambda name e) = LambdaBox name (langToNM e)
langToNM (App e1 e2)     = BinaryBox (langToNM e1) (langToNM e2)

nmToLang :: ExpAsTree -> Exp
nmToLang (Box name)         = Var name
nmToLang (LambdaBox name e) = Lambda name (nmToLang e)
nmToLang (BinaryBox e1 e2)  = App (nmToLang e1) (nmToLang e2)

instance Bijective Exp ExpAsTree where
  toNM   = langToNM
  fromNM = nmToLang

instance Steppable ExpAsTree where
  step = stepNM step

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



newtype Trace s = Trace [s]
instance Pretty s => Pretty (Trace s) where
  pretty (Trace ss) = vsep $ map pretty ss

langPipeline :: LangPipeline Exp () ParseError [Exp]
langPipeline = LangPipeline parse (Right . eval) Nothing (Right . trace)

repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("ascii",       ascii)
    , ("asciiTrace",  asciiTrace)
    , ("render",      render fileName w)
    , ("renderTrace", renderTrace fileName w)
    ] "ExpressionTree>" helpMsg langPipeline
  where helpMsg = "Play with the Expression as Tree notional machine for Untyped Lambda Calculus"

ascii :: String -> IO ()
ascii =       mkAsciiTraceCmd . fmap return . str2NM

asciiTrace :: String -> IO ()
asciiTrace =  mkAsciiTraceCmd . str2NMTrace

render :: String -> Int -> String -> IO ()
render fileName w =      either print (renderNM fileName w) . str2NM

renderTrace :: String -> Int -> String -> IO ()
renderTrace fileName w = either print (renderNMSeq fileName w) . str2NMTrace

----- Helpers -----

str2NM :: String -> Either ParseError ExpAsTree
str2NM = fmap langToNM . parse

str2NMTrace :: String -> Either ParseError [ExpAsTree]
str2NMTrace = fmap trace . str2NM

mkAsciiTraceCmd :: Either ParseError [ExpAsTree] -> IO ()
mkAsciiTraceCmd = mkCmd . fmap (Trace . map toAscii)

renderNMSeq :: String -> Int -> [ExpAsTree] -> IO ()
renderNMSeq fileName w = renderDiagram fileName w . vDiaSeq 1.5 0.5 . map (toDiagram 1)

renderNM :: String -> Int -> ExpAsTree -> IO ()
renderNM fileName w = renderDiagram fileName w . toDiagram 1

renderDiagram :: String -> Int -> Diagram B -> IO ()
renderDiagram fileName w = renderDiagramRaster fileName w . bgFrame 0.05 white

