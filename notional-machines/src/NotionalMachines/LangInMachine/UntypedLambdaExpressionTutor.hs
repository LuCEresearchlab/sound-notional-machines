{-# OPTIONS_GHC -Wall -Wno-missing-pattern-synonym-signatures -Wno-orphans #-}

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

module NotionalMachines.LangInMachine.UntypedLambdaExpressionTutor where

import Control.Monad.State.Lazy (State, StateT (..), lift)

import Data.Set (Set)

import Text.ParserCombinators.Parsec (ParseError)

import qualified Hedgehog.Gen as Gen

import NotionalMachines.Lang.UntypedLambda.Generators (genCombinator, genExp)
import NotionalMachines.Lang.UntypedLambda.Main       (Exp (..), parse, unparse)

import NotionalMachines.Machine.ExpressionTutor.Main (ExpTutorDiagram (..), NodeContentElem (..),
                                                      checkCycle, etToLang, holeP, langToET,
                                                      newDiaBranch, newDiaLeaf, pattern DiaBranch,
                                                      pattern DiaLeaf, pattern MkNode)

import NotionalMachines.Meta.Simulation (Simulation, mkInjSim)
import NotionalMachines.Meta.Injective    (Injective, fromNM, toNM)
import NotionalMachines.Meta.Steppable    (eval, step)


pattern NodeVar    name i <- MkNode i _       [NameUse name] where
        NodeVar    name i =  MkNode i Nothing [NameUse name]
pattern NodeLambda name i <- MkNode i _       [C "lambda", NameDef name, Hole {}] where
        NodeLambda name i =  MkNode i Nothing [C "lambda", NameDef name, holeP]
pattern NodeApp         i <- MkNode i _       [Hole {}, Hole {}] where
        NodeApp         i =  MkNode i Nothing [holeP,   holeP]

lambdaToET :: Exp -> ExpTutorDiagram
lambdaToET = langToET go
  where go :: Exp -> State Int ExpTutorDiagram
        go = \case
          Var name      -> newDiaLeaf   (NodeVar    name)
          Lambda name e -> newDiaBranch (NodeLambda name) go [e]
          App e1 e2     -> newDiaBranch  NodeApp          go [e1, e2]


etToLambda :: ExpTutorDiagram -> Maybe Exp
etToLambda = etToLang go
  where
    -- traverse diagram to build Exp keeping track of visited nodes to not get stuck
    go :: ExpTutorDiagram -> StateT (Set Int) Maybe Exp
    go d = checkCycle d $ case d of
      DiaLeaf   (NodeVar    name _)          -> return (Var name)
      DiaBranch (NodeLambda name _) [n]      -> Lambda name <$> go n
      DiaBranch (NodeApp    _)      [n1, n2] -> App <$> go n1 <*> go n2
      _                                      -> lift Nothing -- "incorrect diagram"


instance Injective Exp ExpTutorDiagram where
  toNM   = lambdaToET
  fromNM = etToLambda

sim :: Simulation Exp Exp ExpTutorDiagram (Maybe ExpTutorDiagram)
sim = mkInjSim step
-- sim = Sim { fLang  = step
--               , fNM    = stepM
--               , alphaA = toNM
--               , alphaB = return . toNM }

------------------
-- Expression Tutor activities
------------------

---- Parse activity ----

genLambda :: IO String
genLambda = unparse <$> Gen.sample genExp

solveParseActivity :: String -> Either ParseError ExpTutorDiagram
solveParseActivity = fmap toNM . parse


---- Unparse activity ----

generateUnparseActivity :: IO ExpTutorDiagram
generateUnparseActivity = toNM <$> Gen.sample genExp

solveUnparseActivity :: ExpTutorDiagram -> Maybe String
solveUnparseActivity = fmap unparse . fromNM


---- Eval activity ----

generateEvalActivity :: IO String
generateEvalActivity = unparse <$> Gen.sample genCombinator

solveEvalActivity :: String -> Either ParseError ExpTutorDiagram
solveEvalActivity = fmap (toNM . eval) . parse


