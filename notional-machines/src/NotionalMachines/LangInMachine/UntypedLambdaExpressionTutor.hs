{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures -Wno-orphans #-}

{-# LANGUAGE PatternSynonyms, MultiParamTypeClasses, LambdaCase #-}

module NotionalMachines.LangInMachine.UntypedLambdaExpressionTutor where

import Control.Monad.State.Lazy (State, StateT(..), lift)

import Data.Set (Set)

import NotionalMachines.Lang.UntypedLambda.Main (Exp(..))
import NotionalMachines.Machine.ExpressionTutor.Main (ExpTreeDiagram(..), NodeContentElem(..), pattern MkNode, pattern DiaBranch, pattern DiaLeaf, newDiaBranch, newDiaLeaf, etToLang, langToET, checkCycle, holeP)

import NotionalMachines.Meta.Injective (Injective, toNM, fromNM)
import NotionalMachines.Meta.Bisimulation (Bisimulation, mkInjBisim)
import NotionalMachines.Meta.Steppable (step)


pattern NodeVar    name i <- MkNode i _       [NameUse name] where
        NodeVar    name i =  MkNode i Nothing [NameUse name]
pattern NodeLambda name i <- MkNode i _       [C "lambda", NameDef name, Hole {}] where
        NodeLambda name i =  MkNode i Nothing [C "lambda", NameDef name, holeP]
pattern NodeApp         i <- MkNode i _       [Hole {}, Hole {}] where
        NodeApp         i =  MkNode i Nothing [holeP,   holeP]

lambdaToET :: Exp -> ExpTreeDiagram
lambdaToET = langToET go
  where go :: Exp -> State Int ExpTreeDiagram
        go = \case
          Var name      -> newDiaLeaf   (NodeVar    name)
          Lambda name e -> newDiaBranch (NodeLambda name) go [e]
          App e1 e2     -> newDiaBranch  NodeApp          go [e1, e2]


etToLambda :: ExpTreeDiagram -> Maybe Exp
etToLambda = etToLang go
  where
    -- traverse diagram to build Exp keeping track of visited nodes to not get stuck
    go :: ExpTreeDiagram -> StateT (Set Int) Maybe Exp
    go d = checkCycle d $ case d of
      DiaLeaf   (NodeVar    name _)          -> return (Var name)
      DiaBranch (NodeLambda name _) [n]      -> Lambda name <$> go n
      DiaBranch (NodeApp    _)      [n1, n2] -> App <$> go n1 <*> go n2
      _ -> lift Nothing -- "incorrect diagram"


instance Injective Exp ExpTreeDiagram where
  toNM   = lambdaToET
  fromNM = etToLambda

bisim :: Bisimulation Exp Exp ExpTreeDiagram (Maybe ExpTreeDiagram)
bisim = mkInjBisim step
-- bisim = Bisim { fLang  = step
--               , fNM    = stepM
--               , alphaA = toNM
--               , alphaB = return . toNM }

