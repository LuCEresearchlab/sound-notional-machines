{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Machine.ExpressionTutorGenerator where

import qualified Hedgehog.Gen as Gen

import NotionalMachines.Lang.UntypedLambda
import NotionalMachines.Lang.UntypedLambdaExpressionTutor ()
import NotionalMachines.Lang.UntypedLambdaGenerator

import NotionalMachines.Machine.ExpressionTutor

import NotionalMachines.Meta.Injective
import NotionalMachines.Meta.Steppable

------------------
-- Expression Tutor activities
------------------

---- Parse activity ----

genLambda :: IO String
genLambda = unparse <$> Gen.sample genExp

solveParseActivity :: String -> Maybe ExpTreeDiagram
solveParseActivity = fmap toNM . parse


---- Unparse activity ----

generateUnparseActivity :: IO ExpTreeDiagram
generateUnparseActivity = toNM <$> Gen.sample genExp

solveUnparseActivity :: ExpTreeDiagram -> Maybe String
solveUnparseActivity = fmap unparse . fromNM


---- Eval activity ----

generateEvalActivity :: IO String
generateEvalActivity = unparse <$> Gen.sample genCombinator

solveEvalActivity :: String -> Maybe ExpTreeDiagram
solveEvalActivity = fmap (toNM . eval) . parse


