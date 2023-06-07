{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}

module NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs where

import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude     (Any, Diagram, QDiagram, V2)

import NotionalMachines.Lang.Error              (Error)
import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse)

import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color (..))
import NotionalMachines.Machine.AlligatorEggs.Diagram     (toDiagram, toDiagramSeq)
import NotionalMachines.Machine.AlligatorEggs.Main        (AlligatorFamily, AlligatorFamilyF (..),
                                                           deBruijnAlligators)

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Steppable    (Steppable (trace), eval, evalM)

import NotionalMachines.Util.Diagrams (renderD)
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

langPipeline :: LangPipeline Exp () Error [Exp]
langPipeline = LangPipeline parse (Right . eval) Nothing (Right . trace)

-- | Start a REPL for tha Alligator Eggs notional machine.
-- It has both ascii art and svg rendering as concrete representations.
-- The svg output goes to a file given as argument scaled to be rendered with @w@ pixels.
-- TODO: Reduce code duplication between this and the other repls
repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("ascii",       ascii)
    , ("asciiTrace",  asciiTrace)
    , ("render",      r . diagram)
    , ("renderTrace", r . diagramTrace)
    ] "Alligator>" helpMsg langPipeline
  where helpMsg = "Play with the Alligator Eggs notional machine for Lambda Calculus"
        r = renderD renderSVG fileName w

ascii :: String -> IO ()
ascii =       mkCmd . fmap (: []) . str2NM

asciiTrace :: String -> IO ()
asciiTrace =  mkCmd . fmap trace  . str2NM

diagram :: _ => String -> IO (Either Error (QDiagram b V2 Double Any))
diagram = mapM toDiagram . str2NM

diagramTrace :: _ => String -> IO (Either Error (Diagram b))
diagramTrace = mapM toDiagramSeq . fmap trace . str2NM

str2NM :: String -> Either Error [AlligatorFamily]
str2NM = fmap langToNm . parse

