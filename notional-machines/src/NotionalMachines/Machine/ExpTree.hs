{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.Machine.ExpTree where

import NotionalMachines.Lang.UntypedLambda (Exp(..))

import NotionalMachines.Meta.Steppable
import NotionalMachines.Meta.Bijective
import NotionalMachines.Meta.Bisimulation

data ExpAsTree = Box String
               | BinaryBox ExpAsTree ExpAsTree
               | LambdaBox String ExpAsTree
               deriving (Eq, Show)

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

