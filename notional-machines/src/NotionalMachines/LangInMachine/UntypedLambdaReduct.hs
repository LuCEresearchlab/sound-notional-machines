{-# OPTIONS_GHC -Wall -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NotionalMachines.LangInMachine.UntypedLambdaReduct where

import NotionalMachines.Lang.UntypedLambda.Main (Exp (..))
import NotionalMachines.Machine.Reduct.Main     (ReductExp, ReductExpF (..), updateUids)

import NotionalMachines.Meta.Simulation (Simulation, mkInjSim, stepMNM)
import NotionalMachines.Meta.Injective    (Injective, fromNM, toNM)
import NotionalMachines.Meta.Steppable    (SteppableM, step, stepM)


--------------------
-- Lang to NM and back
--------------------
nmToLang :: ReductExp -> Maybe Exp
nmToLang (HolePlug n1 n2  _) = App <$> (nmToLang =<< n1) <*> (nmToLang =<< n2)
nmToLang (HolePipe name n _) = Lambda name <$> (nmToLang =<< n)
nmToLang (Pipe name       _) = Just (Var name)

langToNm :: Exp -> ReductExp
langToNm p = updateUids 0 (go p 0)
  where go (App e1 e2)     = HolePlug (Just (langToNm e1)) (Just (langToNm e2))
        go (Lambda name e) = HolePipe name (Just (langToNm e))
        go (Var name)      = Pipe name

------------------

--    A  --f-->  B
--
--    ^          ^
--    |          |
--  alphaA    alphaB
--    |          |
--    |          |
--
--    A' --f'--> B'

instance Injective Exp ReductExp where
  toNM   = langToNm
  fromNM = nmToLang

instance SteppableM ReductExp Maybe where
  stepM = stepMNM (step :: Exp -> Exp)

sim :: Simulation Exp Exp ReductExp (Maybe ReductExp)
sim = mkInjSim step
-- sim = Sim { fLang  = eval
--               , fNM    = evalM
--               , alphaA = toNM
--               , alphaB = return . toNM }

