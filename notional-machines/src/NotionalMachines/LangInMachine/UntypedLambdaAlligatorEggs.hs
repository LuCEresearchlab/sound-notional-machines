{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs where

import Control.Monad ((<=<))

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse)

import NotionalMachines.Machine.AlligatorEggs.AsciiSyntax (toAscii)
import NotionalMachines.Machine.AlligatorEggs.Diagram     (toDiagram)
import NotionalMachines.Machine.AlligatorEggs.Main        (AlligatorFamily, AlligatorFamilyF (..),
                                                           deBruijnAlligators)
import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color (..))

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Steppable    (Steppable (trace), eval, evalM)

import NotionalMachines.Utils (LangPipeline (LangPipeline), diaSeq, mkCmd, mkLangReplOpts,
                               renderDiagram)

import Text.Parsec (ParseError)

import Diagrams.Backend.SVG (SVG)
import Diagrams.Prelude     (Diagram, bgFrame, white)
import Prettyprinter        (Pretty, pretty, vsep)

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
    [ ("ascii",       mkAsciiTraceCmd . fmap return . str2Alligator)
    , ("asciiTrace",  mkAsciiTraceCmd . str2AlligatorTrace)
    , ("render",      either print renderAlligators . str2Alligator)
    , ("renderTrace", either print renderAlligatorSeq . str2AlligatorTrace)
    ] "Alligator>" helpMsg langPipeline
  where helpMsg = "Play with the Alligator Eggs notional machine for Lambda Calculus"

        str2Alligator :: String -> Either ParseError [AlligatorFamily]
        str2Alligator = fmap langToNm . parse

        str2AlligatorTrace :: String -> Either ParseError [[AlligatorFamily]]
        str2AlligatorTrace = fmap trace . str2Alligator

        mkAsciiTraceCmd :: Either ParseError [[AlligatorFamily]] -> IO ()
        mkAsciiTraceCmd = mkCmd . fmap (Trace . map toAscii)

        renderAlligatorSeq :: [[AlligatorFamily]] -> IO ()
        renderAlligatorSeq = render <=< fmap (diaSeq 6 0.9 0.5) . mapM (toDiagram 1)

        renderAlligators :: [AlligatorFamily] -> IO ()
        renderAlligators = render <=< toDiagram 1

        render :: Diagram SVG -> IO ()
        render = renderDiagram fileName (fromIntegral w) . bgFrame 0.05 white
