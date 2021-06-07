{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import           Hedgehog hiding (Var, eval)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Main (defaultMain)

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)

import Data.List (nub)

import Data.Functor.Identity (Identity)

import Text.Show.Pretty (pPrint, ppDoc)
import Text.PrettyPrint (Doc)

import Lib


----- Generators -----

genName :: MonadGen m => m String
genName = Gen.list (Range.singleton 1) $ Gen.element ['a'..'c']

genExp :: MonadGen m => m Exp
genExp =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> genName
    ] [
      -- recursive generators
      Gen.subtermM genExp (\x -> Lambda <$> genName <*> pure x)
    , Gen.subterm2 genExp genExp App
    ]

genCombinator :: (MonadGen m, GenBase m ~ Identity) => m Exp
genCombinator = fmap bindFreeVars genExp

bindFreeVars :: Exp -> Exp
bindFreeVars e = foldl (\en var -> App (Lambda var en) (Lambda "a" (Var "a"))) e (freeVs e)

genReductExp :: MonadGen m => m ReductTerm
genReductExp =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Pipe <$> genName
    ] [
      -- recursive generators
      Gen.subtermM genReductExp (\x -> HolePipe <$> genName <*> Gen.maybe (pure (x, 0)))
    , Gen.subtermM2 genReductExp genReductExp
        (\x y -> HolePlug <$> (Gen.maybe (pure (x, 0))) <*> (Gen.maybe (pure (y, 0))))
    ]

----------------------

----- Properties -----

prop_eval_to_value :: Property
prop_eval_to_value = property $ do
  e <- forAll genCombinator
  (isValue $ eval e) === True

isValue :: Exp -> Bool
isValue Lambda {} = True
isValue _         = False

-- prop_eval_equiv_step :: Property
-- prop_eval_equiv_step = property $ do
--   e <- forAll genCombinator
--   eval e === bigStep e

prop_parse_is_inverse_upparse :: Property
prop_parse_is_inverse_upparse = property $ do
  e <- forAll genExp
  tripping e unparse parse

prop_lang2nm_is_inverse_nm2lang :: Property
prop_lang2nm_is_inverse_nm2lang = property $ do
  e <- forAll genExp
  tripping e lang2nm nm2lang

prop_commutation :: Property
prop_commutation = property $ do
  e <- forAll genCombinator
  let expEnv = unparse e
  (alphaB . f') expEnv === (f . alphaA) expEnv

prop_uniqids :: Property
prop_uniqids = property $ do
  e <- forAll genReductExp
  let updatedExp = updateUids 0 (e, 0)
  footnoteShow updatedExp
  let ids = uids updatedExp
  ids === [1..(length ids)]

uids = foldMM (\(t, id) -> [id])

----------------------

----- Run -----

tests :: IO Bool
tests =
  checkParallel $ Group "Test.All" [
        ("eval returns a value:", prop_eval_to_value)
      -- , ("eval is equivalent to bigStep:", prop_eval_equiv_step)
      , ("parse is the inverse of unparse:", prop_parse_is_inverse_upparse)
      , ("lang2nm is the inverse of nm2lang:", prop_lang2nm_is_inverse_nm2lang)
      , ("commutation proof:", prop_commutation)
      , ("unique ids:", prop_uniqids)
    ]


main :: IO ()
main = defaultMain [tests]




------- Ganerate activities ----------

sample :: Show a => Int -> Gen a -> IO ()
sample n gen = forM_ [1..n] (\_ -> pPrint =<< Gen.sample gen)

generateParseActivity :: MonadIO m => m String
generateParseActivity = unparse <$> Gen.sample genExp

generateUnparseActivity :: MonadIO m => m ReductExp
generateUnparseActivity = lang2nm <$> Gen.sample genExp

generateEvalActivity :: MonadIO m => m String
generateEvalActivity = unparse <$> Gen.sample genCombinator

genAndSolve :: (Show a, Show b) => IO a -> (a -> b) -> IO Doc
genAndSolve gen solver =
  ppDoc <$> do e <- gen
               pPrint e
               return $ solver e

