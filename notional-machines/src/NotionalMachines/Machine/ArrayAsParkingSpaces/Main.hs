{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}


module NotionalMachines.Machine.ArrayAsParkingSpaces.Main where

import           Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State.Lazy (StateT, get, modify, lift)

import NotionalMachines.Util.Util (nextKey)


---- Sublanguage needed for the parking space nm

data Type = TyNat
          | TyBool
          | TyOtherPrim
          | TyRef Type
  deriving (Eq, Show)

data Term = PrimValue String
          | Loc Location
          | ArrayAlloc Type Int Id
          | ArrayWrite Term Int Term
          | ArrayAccess Term Int
          | Unit
          | Null
          | Other
  deriving (Eq, Show)

type Location = Int
type Id = String

isValue :: Term -> Bool
isValue = \case
  PrimValue _ -> True
  Loc _       -> True
  Unit        -> True
  Null        -> True
  _           -> False
-------------------------------

data Problem = Problem String
  deriving (Show)

type ParkingSpace = Map Address [Spot]
type Address = Location
type Spot = Maybe Car
newtype Car = Car Term
  deriving (Show)

emptyParkingSpace :: ParkingSpace
emptyParkingSpace = Map.empty

problem = lift . Left . Problem

----- Parking space manipulation -----

-- allocate
addSpots :: Type -> Int -> Id -> StateT ParkingSpace (Either Problem) Location
addSpots ty size id = do
  let loc = read id
  modify (Map.insert loc (replicate size . termToParkingSpot . defaultVal $ ty))
  return loc

defaultVal :: Type -> Term
defaultVal = \case
  TyNat     -> PrimValue "0"
  TyBool    -> PrimValue "false"
  TyOtherPrim -> PrimValue "other0"
  TyRef _   -> Null

termToParkingSpot :: Term -> Spot
termToParkingSpot = \case
  Null        -> Nothing
  t           -> Just (Car t)

-- dereference
getSpot :: Location -> Int -> StateT ParkingSpace (Either Problem) Term
getSpot loc i = do
  ps <- get
  case Map.lookup loc ps of
    Nothing -> problem "no such location"
    Just spots -> case spots !! i of
      Nothing -> problem "no such spot"
      Just (Car v) -> return v

-- assign
setSpot :: Location -> Int -> Term -> StateT ParkingSpace (Either Problem) ()
setSpot loc i v = getSpot loc i *> modify (Map.adjust (replace i (termToParkingSpot v)) loc)

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs
-------------------------------------


fNM :: Term -> StateT ParkingSpace (Either Problem) Term
fNM t = case t of
  ArrayAlloc ty size id    -> Loc <$> addSpots ty size id
  ArrayWrite (Loc loc) i v -> setSpot loc i v >> return Unit
  ArrayWrite Null _ _      -> problem "null dereference"
    -- careful with "spot already taken" miscon
    -- "no such location" is good
  ArrayAccess (Loc loc) i  -> getSpot loc i
  ArrayAccess Null _       -> problem "null dereference"
  _                        -> return Other


fNM_Replay :: [Term] -> StateT ParkingSpace (Either Problem) Term
fNM_Replay [] = problem "no replay"
