{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Machine.ExpressionTutor.Generators where

import           Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import NotionalMachines.Lang.UntypedLambda.Main
import NotionalMachines.Lang.UntypedLambda.Generators

import NotionalMachines.Machine.ExpressionTutor.Main

import NotionalMachines.LangInMachine.UntypedLambdaExpressionTutor ()

import NotionalMachines.Meta.Injective
import NotionalMachines.Meta.Steppable

genExpTreeDiagram :: MonadGen m => m ExpTreeDiagram
genExpTreeDiagram = do n <- Gen.set (Range.linear 0 5) genNode
                       e <- Gen.set (Range.linear 0 5) genEdge
                       r <- Gen.maybe genNode
                       return $ ExpTreeDiagram n e r

genNode :: MonadGen m => m Node
genNode = do n <- genPlug
             t <- Gen.maybe genType
             c <- Gen.list (Range.linear 0 5) genNodeContentElem
             return $ Node n t c

genNodeContentElem :: MonadGen m => m NodeContentElem
genNodeContentElem = Gen.choice [
                         C <$> genString
                       , NameDef <$> genString
                       , NameUse <$> genString
                       , Hole <$> genPlug
                       ]

genEdge :: MonadGen m => m Edge
genEdge = Edge <$> genPlug <*> genPlug

genPlug :: MonadGen m => m Plug
genPlug = do n1 <- Gen.int (Range.linear 0 100)
             n2 <- Gen.int (Range.linear 0 100)
             return $ Plug (n1, n2)

genType :: MonadGen m => m Type
genType = genString

genString :: MonadGen m => m String
genString = Gen.list (Range.linear 0 10) $ Gen.element ['a'..'z']

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


