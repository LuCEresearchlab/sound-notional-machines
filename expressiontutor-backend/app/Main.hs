{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger

import Data.Aeson (ToJSON)

import Data.Text.Lazy (pack)

import NotionalMachines.Machine.ExpressionTutor.Main (ExpTutorDiagram)

import qualified NotionalMachines.Lang.UntypedArith.Main as Arith (Term, parse, unparse)
import           NotionalMachines.Lang.UntypedArith.Generators (genTerm)

import qualified NotionalMachines.Lang.UntypedLambda.Main as Lambda (Exp, parse, unparse)
import           NotionalMachines.Lang.UntypedLambda.Generators (genExp)

import           NotionalMachines.LangInMachine.UntypedLambdaExpressionTutor ()
import           NotionalMachines.LangInMachine.UntypedArithExpressionTutor ()

import NotionalMachines.Meta.Bisimulation (stepMNM)
import NotionalMachines.Meta.Injective (toNM)
import NotionalMachines.Meta.Steppable (step, eval)

import NotionalMachines.Utils (sample)

import Lib ()

main :: IO ()
main = scotty 3000 $ do

  middleware logStdoutDev

  get "/" (html "ExpressionTutor backend for lambda calculus")

  get "/arith/gen" $ do
    e <- liftAndCatchIO (sample genTerm)
    (text . pack . Arith.unparse) e
  get "/arith/step" $ do
    e <- param "e"
    (erHandlerText . fmap (Arith.unparse . step) . Arith.parse) e
  get "/arith/eval" $ do
    e <- param "e"
    (erHandlerText . fmap (Arith.unparse . eval) . Arith.parse) e

  get "/lambda/gen" $ do
    e <- liftAndCatchIO (sample genExp)
    (text . pack . Lambda.unparse) e
  get "/lambda/step" $ do
    e <- param "e"
    (erHandlerText . fmap (Lambda.unparse . step) . Lambda.parse) e
  get "/lambda/eval" $ do
    e <- param "e"
    (erHandlerText . fmap (Lambda.unparse . eval) . Lambda.parse) e

  get "/et/arith" $ do
    e <- param "e"
    (erHandlerJson . fmap (toNM :: Arith.Term -> ExpTutorDiagram) . Arith.parse) e
  post "/et/arith/step" $ do
    d :: ExpTutorDiagram <- jsonData
    (json . stepMNM (step :: Arith.Term -> Arith.Term)) d
  get "/et/lambda" $ do
    e <- param "e"
    (erHandlerJson . fmap (toNM :: Lambda.Exp -> ExpTutorDiagram) . Lambda.parse) e
  post "/et/lambda/step" $ do
    d :: ExpTutorDiagram <- jsonData
    (json . stepMNM (step :: Lambda.Exp -> Lambda.Exp)) d

erHandlerText :: Show e => Either e String -> ActionM ()
erHandlerText =  either (handle (g . show)) g
  where g = text . pack

erHandlerJson :: (Show e, ToJSON a) => Either e a -> ActionM ()
erHandlerJson = either (handle (text . pack . show)) json

handle :: (a -> ActionM ()) -> a -> ActionM ()
handle f e = status status400 >> f e
