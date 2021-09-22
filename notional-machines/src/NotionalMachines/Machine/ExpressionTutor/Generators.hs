{-# OPTIONS_GHC -Wall #-}

module NotionalMachines.Machine.ExpressionTutor.Generators where

import           Hedgehog       (MonadGen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range


import NotionalMachines.Machine.ExpressionTutor.Main

import NotionalMachines.LangInMachine.UntypedLambdaExpressionTutor ()


genExpTutorDiagram :: MonadGen m => m ExpTutorDiagram
genExpTutorDiagram = do n <- Gen.set (Range.linear 0 5) genNode
                        e <- Gen.set (Range.linear 0 5) genEdge
                        r <- Gen.maybe genNode
                        return $ ExpTutorDiagram n e r

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

