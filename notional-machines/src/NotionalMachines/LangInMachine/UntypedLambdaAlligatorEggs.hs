{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs where

import Control.Monad ((<=<))

import Data.List (intersperse)

import NotionalMachines.Lang.UntypedLambda.Main    (Exp (..), parse)

import NotionalMachines.Machine.AlligatorEggs.Diagram (toDiagram)
import NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (toAscii)
import NotionalMachines.Machine.AlligatorEggs.Main (AlligatorFamily,
                                                    AlligatorFamilyF (..), Color,
                                                    deBruijnAlligators, nameToColor)

import NotionalMachines.Meta.Simulation (Simulation (..))
import NotionalMachines.Meta.Steppable    (eval, evalM, Steppable (trace))

import NotionalMachines.Utils ( mkLangReplOpts, LangPipeline(LangPipeline), renderDiagram, diaSeq, mkCmd )

import Text.Parsec (ParseError)

import Prettyprinter (Pretty, pretty, line, vsep)
import Diagrams.Prelude (white, bgFrame)

-------------------------
-- Lang to NM and back --
-------------------------
langToNm :: Exp -> [AlligatorFamily]
langToNm (Var name)            = [Egg (nameToColor name)]
langToNm (Lambda name e)       = [HungryAlligator (nameToColor name) (langToNm e)]
langToNm (App e1 e2@(App _ _)) = langToNm e1 ++ [OldAlligator (langToNm e2)]
langToNm (App e1 e2)           = langToNm e1 ++ langToNm e2

sim :: Simulation Exp Exp [AlligatorFamilyF Color] (Maybe [AlligatorFamilyF Int])
sim = MkSim { fLang  = eval
            , fNM    = fmap deBruijnAlligators . evalM
            , alphaA = langToNm
            , alphaB = return . deBruijnAlligators . langToNm }


-------------------------
-- REPL -----------------
-------------------------

newtype Trace s = Trace [s]
instance Pretty s => Pretty (Trace s) where
  pretty (Trace ss) = vsep $ map pretty ss

langPipeline :: LangPipeline Exp () ParseError [Exp]
langPipeline = LangPipeline parse (Right . eval) Nothing (Right . trace)

-- | Start a REPL for tha Alligator Eggs notional machine. It has both ascii art and svg rendering as concrete representations. The svg output goes to a file given as argument scaled to be rendered with @w@ pixels.
repl :: FilePath -> Double -> IO ()
repl fileName w = mkLangReplOpts
    [ ("ascii", mkCmd . fmap (toAscii . langToNm) . parse)
    , ("asciiTrace", mkCmd . fmap (Trace . map (toAscii . langToNm) . trace) . parse)
    , ("render", either print (renderAlligators . langToNm) . parse)
    , ("renderTrace", either print (renderAlligatorSeq . map langToNm . trace) . parse)
    ] "Alligator>" helpMsg langPipeline
  where helpMsg = "Play with the Alligator Eggs notional machine for Lambda Calculus"
        renderAlligatorSeq = render <=< fmap (diaSeq 4 0.9 0.5) . mapM (toDiagram 1)
        renderAlligators = render <=< toDiagram 1
        render = renderDiagram fileName w . bgFrame 0.05 white

