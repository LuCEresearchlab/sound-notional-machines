{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.UntypedLambdaExpressionTree where

import NotionalMachines.Lang.UntypedLambda.Main     (Exp (..))
import NotionalMachines.Machine.ExpressionTree.Main (ExpAsTree (..))

import NotionalMachines.Meta.Bijective    (Bijective, fromNM, toNM)
import NotionalMachines.Meta.Simulation (Simulation, mkBijSim, stepNM)
import NotionalMachines.Meta.Steppable    (Steppable, step)

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

sim :: Simulation Exp Exp ExpAsTree ExpAsTree
sim = mkBijSim
-- sim = Sim { fLang  = eval
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


