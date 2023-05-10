{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.UntypedLambdaExpressionTree (
      bisim
    , nmToLang

    , repl
    , ascii
    , asciiTrace
    , render
    , renderTrace
    ) where

import Control.Monad ((<=<))

import Text.Parsec (ParseError)

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..), parse)

import NotionalMachines.Machine.ExpressionTree.Diagram (renderDiagram)
import NotionalMachines.Machine.ExpressionTree.Main    (ExpAsTree (..))

import NotionalMachines.Meta.Bijective    (Bijective, fromNM)
import NotionalMachines.Meta.Bisimulation (Bisimulation, mkBijBisim, mkStepBijNM)
import NotionalMachines.Meta.Diagramable  (Diagramable (..))
import NotionalMachines.Meta.LangToNM     (LangToNM (..))
import NotionalMachines.Meta.Steppable    (Steppable, eval, step, trace)

import NotionalMachines.Util.REPL (LangPipeline (..), mkCmd, mkLangReplOpts)

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



langPipeline :: LangPipeline Exp () ParseError [Exp]
langPipeline = LangPipeline parse (Right . eval) Nothing (Right . trace)

-- TODO: Reduce code duplication between this and the other repls
repl :: FilePath -> Int -> IO ()
repl fileName w = mkLangReplOpts
    [ ("ascii",       ascii)
    , ("asciiTrace",  asciiTrace)
    , ("render",      render fileName w)
    , ("renderTrace", renderTrace fileName w)
    ] "ExpressionTree>" helpMsg langPipeline
  where helpMsg = "Play with the Expression as Tree notional machine for Untyped Lambda Calculus"

ascii :: String -> IO ()
ascii =       mkCmd . fmap (: []) . str2NM

asciiTrace :: String -> IO ()
asciiTrace =  mkCmd . fmap trace  . str2NM

render :: FilePath -> Int -> String -> IO ()
render fileName w =      either print (renderDiagram fileName w <=< toDiagram) . str2NM

renderTrace :: FilePath -> Int -> String -> IO ()
renderTrace fileName w = either print ((renderDiagram fileName w <=< toDiagramSeq) . trace) . str2NM

----- Helpers -----

str2NM :: String -> Either ParseError ExpAsTree
str2NM = fmap langToNM . parse

