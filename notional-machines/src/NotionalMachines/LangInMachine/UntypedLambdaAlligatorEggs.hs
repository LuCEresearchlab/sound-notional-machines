{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.LangInMachine.UntypedLambdaAlligatorEggs where

import Control.Monad ((<=<))

import Text.Parsec (ParseError)

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse)

import NotionalMachines.Machine.AlligatorEggs.ColorAsName (Color (..))
import NotionalMachines.Machine.AlligatorEggs.Diagram     (renderDiagram)
import NotionalMachines.Machine.AlligatorEggs.Main        (AlligatorFamily, AlligatorFamilyF (..),
                                                           deBruijnAlligators)

import NotionalMachines.Meta.Bisimulation (Bisimulation (..))
import NotionalMachines.Meta.Diagramable  (Diagramable (..))
import NotionalMachines.Meta.Steppable    (Steppable (trace), eval, evalM)

import NotionalMachines.Util.REPL (LangPipeline (LangPipeline), mkCmd, mkLangReplOpts)

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

langPipeline :: LangPipeline Exp () ParseError [Exp]
langPipeline = LangPipeline parse (Right . eval) Nothing (Right . trace)

-- | Start a REPL for tha Alligator Eggs notional machine.
-- It has both ascii art and svg rendering as concrete representations.
-- The svg output goes to a file given as argument scaled to be rendered with @w@ pixels.
-- TODO: Reduce code duplication between this and the other repls
repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("ascii",       ascii)
    , ("asciiTrace",  asciiTrace)
    , ("render",      render fileName w)
    , ("renderTrace", renderTrace fileName w)
    ] "Alligator>" helpMsg langPipeline
  where helpMsg = "Play with the Alligator Eggs notional machine for Lambda Calculus"

ascii :: String -> IO ()
ascii =       mkCmd . fmap (: []) . str2NM

asciiTrace :: String -> IO ()
asciiTrace =  mkCmd . fmap trace  . str2NM

render :: FilePath -> Int -> String -> IO ()
render fileName w =      either print (renderDiagram fileName w <=< toDiagram) . str2NM

renderTrace :: FilePath -> Int -> String -> IO ()
renderTrace fileName w = either print ((renderDiagram fileName w <=< toDiagramSeq) . trace) . str2NM

----- Helpers -----

str2NM :: String -> Either ParseError [AlligatorFamily]
str2NM = fmap langToNm . parse

