{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs where

import Control.Monad ((<=<))

import Text.Parsec (ParseError)

import Prettyprinter (Pretty, pretty, vsep)

import Diagrams.Backend.SVG (SVG)
import Diagrams.Prelude     (Diagram, bgFrame, white)

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse)

import NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (toAscii)
import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color (..))
import NotionalMachines.Machine.AlligatorEggs.Diagram     (toDiagram)
import NotionalMachines.Machine.AlligatorEggs.Main        (AlligatorFamily, AlligatorFamilyF (..),
                                                           deBruijnAlligators)

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Steppable    (Steppable (trace), eval, evalM)

import NotionalMachines.Util.Diagrams (diaSeq, renderDiagramSVG)
import NotionalMachines.Util.REPL     (LangPipeline (LangPipeline), mkCmd, mkLangReplOpts)

-------------------------
-- Lang to NM and back --
-------------------------
langToNm :: Exp -> [AlligatorFamily]
langToNm (Var name)            = [Egg (MkColorFromName name)]
langToNm (Lambda name e)       = [HungryAlligator (MkColorFromName name) (langToNm e)]
langToNm (App e1 e2@(App _ _)) = langToNm e1 ++ [OldAlligator (langToNm e2)]
langToNm (App e1 e2)           = langToNm e1 ++ langToNm e2

bisim :: Bisimulation Exp Exp [AlligatorFamilyF Color] (Maybe [AlligatorFamilyF Int])
bisim = MkBisim { fLang  = eval
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

-- | Start a REPL for tha Alligator Eggs notional machine.
-- It has both ascii art and svg rendering as concrete representations.
-- The svg output goes to a file given as argument scaled to be rendered with @w@ pixels.
repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("ascii",       ascii)
    , ("asciiTrace",  asciiTrace)
    , ("render",      render fileName w)
    , ("renderTrace", renderTrace fileName w)
    ] "Alligator>" helpMsg langPipeline
  where helpMsg = "Play with the Alligator Eggs notional machine for Lambda Calculus"

ascii :: String -> IO ()
ascii =       mkAsciiTraceCmd . fmap return . str2NM

asciiTrace :: String -> IO ()
asciiTrace =  mkAsciiTraceCmd . str2NMTrace

render :: FilePath -> Int -> String -> IO ()
render fileName w =      either print (renderNM fileName w) . str2NM

renderTrace :: FilePath -> Int -> String -> IO ()
renderTrace fileName w = either print (renderNMSeq fileName w) . str2NMTrace

----- Helpers -----

str2NM :: String -> Either ParseError [AlligatorFamily]
str2NM = fmap langToNm . parse

str2NMTrace :: String -> Either ParseError [[AlligatorFamily]]
str2NMTrace = fmap trace . str2NM

mkAsciiTraceCmd :: Either ParseError [[AlligatorFamily]] -> IO ()
mkAsciiTraceCmd = mkCmd . fmap (Trace . map toAscii)

renderNMSeq :: FilePath -> Int -> [[AlligatorFamily]] -> IO ()
renderNMSeq fileName w = renderDiagram fileName w <=< fmap (diaSeq 6 0.9 0.5) . mapM (toDiagram 1)

renderNM :: FilePath -> Int -> [AlligatorFamily] -> IO ()
renderNM fileName w = renderDiagram fileName w <=< toDiagram 1

renderDiagram :: FilePath -> Int -> Diagram SVG -> IO ()
renderDiagram fileName w = renderDiagramSVG fileName (fromIntegral w) . bgFrame 0.05 white
