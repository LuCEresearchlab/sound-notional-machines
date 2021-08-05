{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (
  Term(..),
  Type(..),

  Store,
  emptyStore,

  isValue,
  isNumericVal,

  evalM',

  typeof,
  typecheck
  ) where

import Control.Monad.State.Lazy (StateT, get, withStateT, lift, evalStateT)

import Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as Map

import NotionalMachines.Utils (maybeToEither)
import NotionalMachines.Meta.Steppable (SteppableM, stepM, evalM)


--------------------
-- Simply Typed Lambda Calculus + Unit + References + Booleans and Arithmetic Expressions
--------------------
data Type = TyFun Type Type
          | TyUnit
          | TyRef Type
          | TyBool
          | TyNat
          deriving (Eq, Show)

type TypCtx = [(Name, Type)]

-- type TyStore = Map Location Type

data Term = -- Lambdas
            Var Name
          | Lambda Name Type Term
          | App Term Term
            -- Unit
          | Unit
            -- References
          | Ref Term
          | Deref Term
          | Assign Term Term
          | Loc Location
            -- Booleans
          | Tru
          | Fls
          | If Term Term Term
            -- Arithmetic Expressions
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
          deriving (Eq, Show)
type Name = String

------ Store Manipulation ------
type Location = Int
type Store = Map Location Term

emptyStore :: Store
emptyStore = Map.empty

alloc :: Term -> StateT Store (Either String) Term
alloc v = do newLoc <- fmap (succ . foldl max (-1) . Map.keys) get
             withStateT (Map.insert newLoc v) (return $ Loc newLoc) 

deref :: Location -> StateT Store (Either String) Term
deref l = (lift . maybeToEither errorMsg . Map.lookup l) =<< get
  where errorMsg = "error: address not found: " ++ show l

assign :: Location -> Term -> StateT Store (Either String) Term
assign l v = withStateT (Map.insert l v) (return Unit)
-------------------

isValue :: Term -> Bool
isValue = \case
  Lambda {} -> True
  Unit      -> True
  Loc {}    -> True
  Tru       -> True
  Fls       -> True
  t         -> isNumericVal t

isNumericVal :: Term -> Bool
isNumericVal = \case
  Zero   -> True
  Succ t -> isNumericVal t
  _      -> False

typeof :: Term -> Either String Type
typeof = maybeToEither "type error" . typeof' []

typeof' :: TypCtx -> Term -> Maybe Type
typeof' ctx = \case
  -- Lambdas
  Var name                                        -> lookup name ctx     -- T-Var
  Lambda x typ1 (typeof' ((x, typ1):ctx) -> typ2) -> TyFun typ1 <$> typ2 -- T-Abs
  App (rec -> Just (TyFun typ11 typ12))
      (rec -> Just typ2) | typ11 == typ2          -> return typ12        -- T-App
  -- Unit
  Unit                                            -> return TyUnit       -- T-Unit
  -- References
  Ref t                                           -> TyRef <$> rec t     -- T-Ref
  Deref  (rec -> Just (TyRef typ1))               -> return typ1         -- T-Deref
  Assign (rec -> Just (TyRef typ1))
         (rec -> Just typ2) | typ1 == typ2        -> return TyUnit       -- T-Assign
  -- Booleans + Arith
  Tru                                             -> return TyBool       -- T-True
  Fls                                             -> return TyBool       -- T-False
  If t1 t2 t3 | typeOfEq t1 TyBool
             && rec t2 == rec t3                  -> rec t2
  Zero                                            -> return TyNat        -- T-Zero
  Succ t   | typeOfEq t TyNat                     -> return TyNat        -- T-Pred
  Pred t   | typeOfEq t TyNat                     -> return TyNat        -- T-Succ
  IsZero t | typeOfEq t TyNat                     -> return TyBool       -- T-IsZero
  _                                               -> Nothing
  where typeOfEq t1 t2 = rec t1 == Just t2
        rec = typeof' ctx

typecheck :: Term -> Either String (Term, Type)
typecheck t = (t, ) <$> typeof t

instance SteppableM Term (StateT Store (Either String)) where
  stepM = step'

evalM' :: Term -> Either String Term
evalM' t = evalStateT (evalM t) emptyStore

step' :: Term -> StateT Store (Either String) Term
step' = \case
  -- Lambdas
  App t1 t2 | not (isValue t1)     -> (\t1' -> App t1' t2 ) <$> step' t1    -- E-App1
  App v1 t2 | not (isValue t2)     -> (\t2' -> App v1  t2') <$> step' t2    -- E-App2
  App (Lambda name _ t1) t2        -> return $ subst name t2 t1             -- E-AppAbs
  -- References
  Ref v | isValue v                -> alloc v                               -- E-RefV
  Ref t | otherwise                -> Ref   <$> step' t                     -- E-Ref
  Deref (Loc l)                    -> deref l                               -- E-DerefLoc
  Deref t                          -> Deref <$> step' t                     -- E-Deref
  Assign (Loc l) v | isValue v     -> assign l v                            -- E-Assign
  Assign t1 t2 | not (isValue t1)  -> (\t1' -> Assign t1' t2 ) <$> step' t1 -- E-Assign1
  Assign v1 t2 | otherwise         -> (\t2' -> Assign v1  t2') <$> step' t2 -- E-Assign2
  -- Booleans + Arith
  If Tru t2 _                      -> return t2                             -- E-IfTrue
  If Fls _  t3                     -> return t3                             -- E-IfFalse
  If t1  t2 t3                     -> (\t1' -> If t1' t2 t3)   <$> step' t1 -- E-If
  Succ t                           -> Succ   <$> step' t                    -- E-Succ
  Pred Zero                        -> return Zero                           -- E-PredZero
  Pred (Succ v) | isNumericVal v   -> return v                              -- E-PredSucc
  Pred t                           -> Pred   <$> step' t                    -- E-Pred
  IsZero Zero                      -> return Tru                            -- E-IsZeroZero
  IsZero (Succ v) | isNumericVal v -> return Fls                            -- E-IsZeroSucc
  IsZero t                         -> IsZero <$> step' t                    -- E-IsZero
  t                                -> return t

-- Substitute a name by a term in a second term returning the second term with
-- all occurences of the name replaced by the first term. Renaming of variables
-- is performed as need to avoid variable capture.
subst :: Name -> Term -> Term -> Term
subst x v e = case e of
  App e1 e2                            -> App (rec e1) (rec e2)
  Var y | x == y                       -> v
        | otherwise                    -> e
  Lambda y t e2 | x == y               -> e
                | y `notElem` freeVs v -> Lambda y t (rec e2)
                | otherwise            -> Lambda (fresh y) t (rec (subst y (Var (fresh y)) e2))
  If t1 t2 t3                          -> If (rec t1) (rec t2) (rec t3)
  Succ t                               -> Succ (rec t)
  Pred t                               -> Pred (rec t)
  IsZero t                             -> IsZero (rec t)
  Ref t                                -> Ref (rec t)
  Deref t                              -> Deref (rec t)
  Assign t1 t2                         -> Assign (rec t1) (rec t2)
  t                                    -> t
  where rec = subst x v

freeVs :: Term -> [Name]
freeVs = \case
  Var name        -> [name]
  Lambda name _ t -> freeVs t \\ [name]
  App t1 t2       -> freeVs t1 ++ freeVs t2
  If t1 t2 t3     -> freeVs t1 ++ freeVs t2 ++ freeVs t3
  Succ t          -> freeVs t
  Pred t          -> freeVs t
  IsZero t        -> freeVs t
  Ref t           -> freeVs t
  Deref t         -> freeVs t
  Assign t1 t2    -> freeVs t1 ++ freeVs t2
  _               -> []

-- TODO: i think this is incorrect. it doesn't guarantee a global fresh name.
fresh :: Name -> Name
fresh a = "_" ++ a

---

