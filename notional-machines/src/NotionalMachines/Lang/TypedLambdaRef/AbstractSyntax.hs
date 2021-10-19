{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

module NotionalMachines.Lang.TypedLambdaRef.AbstractSyntax (
  Term(..),
  Type(..),

  Error(..),

  -- Expose store manipulation functions for the
  -- memory related notional machines.
  Store,
  Location,
  emptyStore,
  alloc,
  deref,
  assign,
  nextLocation,

  NameEnv,
  emptyNameEnv,

  isValue,
  isNumericVal,

  evalM',

  typeof,
  typecheck
  ) where

import Control.Monad.State.Lazy (State, StateT (StateT), evalStateT, get, lift, runStateT,
                                 withStateT)

import           Data.Bifunctor (first, second)
import           Data.List      ((\\))
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe)

import NotionalMachines.Meta.Steppable (SteppableM, evalM, stepM)
import NotionalMachines.Utils          (maybeToEither, stateToStateT)


--------------------
-- Simply Typed Lambda Calculus + Unit + References + Booleans and Arithmetic Expressions
--------------------
data Error = ParseError String
           | TypeError
           | RuntimeError String
  deriving (Eq, Show)

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
          | Closure NameEnv Name Term
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
type Store l = Map l Term

emptyStore :: Store Location
emptyStore = Map.empty

alloc :: Term -> State (Store Location) Location
alloc v = do newLoc <- fmap nextLocation get
             withStateT (Map.insert newLoc v) (return newLoc)

nextLocation :: (Enum k, Ord k, Num k) => Map k v -> k
nextLocation = succ . foldl max (-1) . Map.keys

deref :: Location -> StateT (Store Location) (Either Error) Term
deref l = (lift . maybeToEither er . Map.lookup l) =<< get
  where er = RuntimeError $ "address not found: " ++ show l

assign :: Location -> Term -> StateT (Store Location) (Either Error) Term
assign l v = deref l *> withStateT (Map.adjust (const v) l) (return Unit) -- try to find first
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

typeof :: Term -> Either Error Type
typeof = maybeToEither TypeError . typeof' []

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

typecheck :: Term -> Either Error (Term, Type)
typecheck t = (t, ) <$> typeof t

----------------------
-- step with explicit name env
----------------------
type NameEnv = Store Name

emptyNameEnv :: NameEnv
emptyNameEnv = Map.empty

instance SteppableM Term (StateT (NameEnv, Store Location) (Either Error)) where
  stepM = stepNameEnv

augmentState :: Monad m => StateT s1 m t -> StateT (s2, s1) m t
augmentState st = StateT (\(s1, s2) -> second (s1, ) <$> runStateT st s2)

isValue' :: Term -> Bool
isValue' Closure {} = True
isValue' Lambda {}  = False
isValue' t          = isValue t

stepNameEnv :: Term -> StateT (NameEnv, Store Location) (Either Error) Term
stepNameEnv = \case
  -- Lambdas
  App t1 t2 | not (isValue' t1)    -> (\t1' -> App t1' t2 ) <$> stepNameEnv t1       -- E-App1
  App v1 t2 | not (isValue' t2)    -> (\t2' -> App v1  t2') <$> stepNameEnv t2       -- E-App2
  -- Here substitution was replaced by explicit naming environment management
  -- Lambda are turned into Closures, which capture the environment.
  Lambda name _ t                  -> (\(env, _) -> Closure env name t) <$> get
  App (Closure env name t1) t2     -> withStateT (first (const (Map.insert name t2 env))) (return t1)
  v @ (Var name)                   -> fromMaybe v . Map.lookup name . fst <$> get
  -- References
  Ref v | isValue' v               -> augmentState $ stateToStateT $ Loc <$> alloc v -- E-RefV
  Ref t | otherwise                -> Ref   <$> stepNameEnv t                        -- E-Ref
  Deref (Loc l)                    -> augmentState $ deref l                         -- E-DerefLoc
  Deref t                          -> Deref <$> stepNameEnv t                        -- E-Deref
  Assign (Loc l) v | isValue' v    -> augmentState $ assign l v                      -- E-Assign
  Assign t1 t2 | not (isValue' t1) -> (\t1' -> Assign t1' t2 ) <$> stepNameEnv t1    -- E-Assign1
  Assign v1 t2 | otherwise         -> (\t2' -> Assign v1  t2') <$> stepNameEnv t2    -- E-Assign2
  -- Booleans + Arith
  If Tru t2 _                      -> return t2                                      -- E-IfTrue
  If Fls _  t3                     -> return t3                                      -- E-IfFalse
  If t1  t2 t3                     -> (\t1' -> If t1' t2 t3)   <$> stepNameEnv t1    -- E-If
  Succ t                           -> Succ   <$> stepNameEnv t                       -- E-Succ
  Pred Zero                        -> return Zero                                    -- E-PredZero
  Pred (Succ v) | isNumericVal v   -> return v                                       -- E-PredSucc
  Pred t                           -> Pred   <$> stepNameEnv t                       -- E-Pred
  IsZero Zero                      -> return Tru                                     -- E-IsZeroZero
  IsZero (Succ v) | isNumericVal v -> return Fls                                     -- E-IsZeroSucc
  IsZero t                         -> IsZero <$> stepNameEnv t                       -- E-IsZero
  t                                -> return t

----------------------
----------------------

----------------------
-- step using substitution
----------------------
instance SteppableM Term (StateT (Store Location) (Either Error)) where
  stepM = step'

evalM' :: Term -> Either Error Term
evalM' t = evalStateT (evalM t) emptyStore

step' :: Term -> StateT (Store Location) (Either Error) Term
step' = \case
  -- Lambdas
  App t1 t2 | not (isValue t1)     -> (\t1' -> App t1' t2 ) <$> step' t1    -- E-App1
  App v1 t2 | not (isValue t2)     -> (\t2' -> App v1  t2') <$> step' t2    -- E-App2
  App (Lambda name _ t1) t2        -> return $ subst name t2 t1             -- E-AppAbs
  -- References
  Ref v | isValue v                -> stateToStateT $ Loc <$> alloc v       -- E-RefV
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

