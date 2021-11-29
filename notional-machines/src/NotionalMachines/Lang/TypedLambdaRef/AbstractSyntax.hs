{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}

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
  StateRacket(..),

  isValue,
  isNumVal,

  evalM',

  typeof,
  typecheck,

  emptyStateAlaWadler,
  emptyStateAlaRacket,

  evalMAlaRacket,
  evalMAlaWadler) where

import Control.Monad.State.Lazy (MonadState (put), State, StateT (StateT), evalStateT, get, lift,
                                 runStateT, withStateT)

import           Data.Bifunctor (first, second)
import           Data.List      ((\\))
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe)


import NotionalMachines.Meta.Steppable (SteppableM, evalM, stepM)
import NotionalMachines.Utils          (Error (..), maybeToEither, stateToStateT, typeOfEq, mismatch)

import Data.Text.Prettyprint.Doc (Doc, Pretty, align, concatWith, hardline, hsep, parens, pretty,
                                  (<+>))


--------------------
-- Simply Typed Lambda Calculus
-- + Unit
-- + Sequence
-- + References
-- + Booleans and Arithmetic Expressions
--------------------
data Type = TyFun Type Type
          | TyUnit
          | TyRef Type
          | TyBool
          | TyNat
          | TyVar Name
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
            -- Sequence
          | Seq Term Term
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
  t         -> isNumVal t

isNumVal :: Term -> Bool
isNumVal = \case
  Zero   -> True
  Succ t -> isNumVal t
  _      -> False

typeof :: Term -> Either Error Type
typeof = typeof' []

typeof' :: TypCtx -> Term -> Either Error Type
typeof' ctx e = case e of
  -- Lambdas
  Var name        -> maybeToEither
                     (TypeError $ "variable '" ++ name ++ "' not in scope.")
                     (lookup name ctx)                                  -- T-Var
  Lambda x typ1 t -> TyFun typ1 <$> typeof' ((x, typ1):ctx) t           -- T-Abs
  App t1 t2       -> do typ1 <- rec t1
                        typ2 <- rec t2
                        case typ1 of
                          TyFun typ11 typ12 -> typeOfEq' t2 typ11 typ12 -- T-App
                          _                 -> mismatch' (TyFun typ2 (TyVar "t")) typ1 t1
  -- Unit
  Unit            -> return TyUnit                                      -- T-Unit
  -- Sequence
  Seq t1 t2       -> rec t1 >> rec t2                                   -- T-Seq
  -- References
  Ref t           -> TyRef <$> rec t                                    -- T-Ref
  Deref t         -> do typ1 <- rec t
                        case typ1 of
                          TyRef typ2 -> return typ2                     -- T-Deref
                          _          -> mismatch' (TyRef (TyVar "t")) typ1 t
  Assign t1 t2    -> do typ1 <- rec t1
                        case typ1 of
                          TyRef typ11 -> typeOfEq' t2 typ11 TyUnit      -- T-Assign
                          _           -> mismatch' (TyRef (TyVar "t")) typ1 t1
  -- Booleans + Arith
  Tru             -> return TyBool                                      -- T-True
  Fls             -> return TyBool                                      -- T-False
  If t1 t2 t3     -> do typ1 <- rec t1
                        typ2 <- rec t2
                        case typ1 of
                          TyBool -> typeOfEq' t3 typ2 typ2              -- T-If
                          _      -> mismatch' TyBool typ1 t1
  Zero            -> return TyNat                                       -- T-Zero
  Succ t          -> typeOfEq' t TyNat TyNat                            -- T-Succ
  Pred t          -> typeOfEq' t TyNat TyNat                            -- T-Pred
  IsZero t        -> typeOfEq' t TyNat TyBool                           -- T-IsZero
  Closure {}      -> Left (TypeError "Closures can't constructed from source code")
  Loc {}          -> Left (TypeError "Locations can't constructed from source code")
  where rec = typeof' ctx
        typeOfEq' = typeOfEq rec e
        mismatch' = mismatch e

typecheck :: Term -> Either Error (Term, Type)
typecheck t = (t, ) <$> typeof t

----------------------
-- step with explicit name env
----------------------
type NameEnv = Store Name

emptyStateAlaWadler :: (NameEnv, Store Location)
emptyStateAlaWadler = (Map.empty, Map.empty)

evalMAlaWadler :: Term -> Either Error Term
evalMAlaWadler t = evalStateT (evalM t) emptyStateAlaWadler

instance SteppableM Term (StateT (NameEnv, Store Location) (Either Error)) where
  stepM = stepAlaWadler

augmentState :: Monad m => StateT s1 m t -> StateT (s2, s1) m t
augmentState st = StateT (\(s1, s2) -> second (s1, ) <$> runStateT st s2)

isValue' :: Term -> Bool
isValue' Closure {} = True
isValue' Lambda {}  = False
isValue' t          = isValue t

stepAlaWadler :: Term -> StateT (NameEnv, Store Location) (Either Error) Term
stepAlaWadler = \case
  -- Lambdas
  App t1 t2 | not (isValue' t1)    -> (\t1' -> App t1' t2 ) <$> stepAlaWadler t1     -- E-App1
  App v1 t2 | not (isValue' t2)    -> (\t2' -> App v1  t2') <$> stepAlaWadler t2     -- E-App2
  -- Here substitution was replaced by explicit naming environment management
  -- Lambda are turned into Closures, which capture the environment.
  Lambda name _ t                  -> (\(env, _) -> Closure env name t) <$> get
  App (Closure env name t1) t2     -> withStateT (first (const (Map.insert name t2 env))) (return t1)
  v @ (Var name)                   -> fromMaybe v . Map.lookup name . fst <$> get
  -- Sequence
  Seq Unit t2                      -> return t2                                      -- E-NextSeq
  Seq t1   t2                      -> (\t1' -> Seq t1' t2) <$> stepAlaWadler t1      -- E-Seq
  -- References
  Ref v | isValue' v               -> augmentState $ stateToStateT $ Loc <$> alloc v -- E-RefV
  Ref t | otherwise                -> Ref   <$> stepAlaWadler t                      -- E-Ref
  Deref (Loc l)                    -> augmentState $ deref l                         -- E-DerefLoc
  Deref t                          -> Deref <$> stepAlaWadler t                      -- E-Deref
  Assign (Loc l) v | isValue' v    -> augmentState $ assign l v                      -- E-Assign
  Assign t1 t2 | not (isValue' t1) -> (\t1' -> Assign t1' t2 ) <$> stepAlaWadler t1  -- E-Assign1
  Assign v1 t2 | otherwise         -> (\t2' -> Assign v1  t2') <$> stepAlaWadler t2  -- E-Assign2
  -- Booleans + Arith
  If Tru t2 _                      -> return t2                                      -- E-IfTrue
  If Fls _  t3                     -> return t3                                      -- E-IfFalse
  If t1  t2 t3                     -> (\t1' -> If t1' t2 t3)   <$> stepAlaWadler t1  -- E-If
  Succ t                           -> Succ   <$> stepAlaWadler t                     -- E-Succ
  Pred Zero                        -> return Zero                                    -- E-PredZero
  Pred (Succ v) | isNumVal v       -> return v                                       -- E-PredSucc
  Pred t                           -> Pred   <$> stepAlaWadler t                     -- E-Pred
  IsZero Zero                      -> return Tru                                     -- E-IsZeroZero
  IsZero (Succ v) | isNumVal v     -> return Fls                                     -- E-IsZeroSucc
  IsZero t                         -> IsZero <$> stepAlaWadler t                     -- E-IsZero
  t                                -> return t

----------------------
----------------------

----------------------
-- step ala racket
----------------------
data StateRacket = StateRacket NameEnv (Store Location)

emptyStateAlaRacket :: StateRacket
emptyStateAlaRacket = StateRacket Map.empty Map.empty

evalMAlaRacket :: Term -> Either Error Term
evalMAlaRacket t = evalStateT (evalM t) emptyStateAlaRacket

instance SteppableM Term (StateT StateRacket (Either Error)) where
  stepM = stepAlaRacket

storeToStateRacket :: Monad m => StateT (Store Location) m t -> StateT StateRacket m t
storeToStateRacket st = StateT (\(StateRacket env store) -> second (StateRacket env) <$> runStateT st store)

stepAlaRacket :: Term -> StateT StateRacket (Either Error) Term
stepAlaRacket = \case
  -- Lambdas
  App t1 t2 | not (isValue t1)    -> (\t1' -> App t1' t2 ) <$> stepAlaRacket t1           -- E-App1
  App v1 t2 | not (isValue t2)    -> (\t2' -> App v1  t2') <$> stepAlaRacket t2           -- E-App2
  App (Lambda name _ t1) t2       -> do StateRacket env s <- get
                                        let newName = until (`Map.notMember` env) fresh name
                                        put $ StateRacket (Map.insert newName t2 env) s
                                        return (subst name (Var newName) t1)
  v @ (Var name)                  -> (\(StateRacket env _) -> fromMaybe v (Map.lookup name env)) <$> get
  -- Sequence
  Seq Unit t2                     -> return t2                                            -- E-NextSeq
  Seq t1   t2                     -> (\t1' -> Seq t1' t2) <$> stepAlaRacket t1            -- E-Seq
  -- References
  Ref v | isValue v               -> storeToStateRacket $ stateToStateT $ Loc <$> alloc v -- E-RefV
  Ref t | otherwise               -> Ref   <$> stepAlaRacket t                            -- E-Ref
  Deref (Loc l)                   -> storeToStateRacket $ deref l                         -- E-DerefLoc
  Deref t                         -> Deref <$> stepAlaRacket t                            -- E-Deref
  Assign (Loc l) v | isValue v    -> storeToStateRacket $ assign l v                      -- E-Assign
  Assign t1 t2 | not (isValue t1) -> (\t1' -> Assign t1' t2 ) <$> stepAlaRacket t1        -- E-Assign1
  Assign v1 t2 | otherwise        -> (\t2' -> Assign v1  t2') <$> stepAlaRacket t2        -- E-Assign2
  -- Booleans + Arith
  If Tru t2 _                     -> return t2                                            -- E-IfTrue
  If Fls _  t3                    -> return t3                                            -- E-IfFalse
  If t1  t2 t3                    -> (\t1' -> If t1' t2 t3)   <$> stepAlaRacket t1        -- E-If
  Succ t                          -> Succ   <$> stepAlaRacket t                           -- E-Succ
  Pred Zero                       -> return Zero                                          -- E-PredZero
  Pred (Succ v) | isNumVal v      -> return v                                             -- E-PredSucc
  Pred t                          -> Pred   <$> stepAlaRacket t                           -- E-Pred
  IsZero Zero                     -> return Tru                                           -- E-IsZeroZero
  IsZero (Succ v) | isNumVal v    -> return Fls                                           -- E-IsZeroSucc
  IsZero t                        -> IsZero <$> stepAlaRacket t                           -- E-IsZero
  t                               -> return t

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
  App t1 t2 | not (isValue t1)    -> (\t1' -> App t1' t2 ) <$> step' t1    -- E-App1
  App v1 t2 | not (isValue t2)    -> (\t2' -> App v1  t2') <$> step' t2    -- E-App2
  App (Lambda name _ t1) t2       -> return $ subst name t2 t1             -- E-AppAbs
  -- Sequence
  Seq Unit t2                     -> return t2                             -- E-NextSeq
  Seq t1   t2                     -> (\t1' -> Seq t1' t2) <$> step' t1     -- E-Seq
  -- References
  Ref v | isValue v               -> stateToStateT $ Loc <$> alloc v       -- E-RefV
  Ref t | otherwise               -> Ref   <$> step' t                     -- E-Ref
  Deref (Loc l)                   -> deref l                               -- E-DerefLoc
  Deref t                         -> Deref <$> step' t                     -- E-Deref
  Assign (Loc l) v | isValue v    -> assign l v                            -- E-Assign
  Assign t1 t2 | not (isValue t1) -> (\t1' -> Assign t1' t2 ) <$> step' t1 -- E-Assign1
  Assign v1 t2 | otherwise        -> (\t2' -> Assign v1  t2') <$> step' t2 -- E-Assign2
  -- Booleans + Arith
  If Tru t2 _                     -> return t2                             -- E-IfTrue
  If Fls _  t3                    -> return t3                             -- E-IfFalse
  If t1  t2 t3                    -> (\t1' -> If t1' t2 t3)   <$> step' t1 -- E-If
  Succ t                          -> Succ   <$> step' t                    -- E-Succ
  Pred Zero                       -> return Zero                           -- E-PredZero
  Pred (Succ v) | isNumVal v      -> return v                              -- E-PredSucc
  Pred t                          -> Pred   <$> step' t                    -- E-Pred
  IsZero Zero                     -> return Tru                            -- E-IsZeroZero
  IsZero (Succ v) | isNumVal v    -> return Fls                            -- E-IsZeroSucc
  IsZero t                        -> IsZero <$> step' t                    -- E-IsZero
  t                               -> return t

-- | Return @e@ with all free occurences of @x@ substituted by @v@a.
-- Renaming of variables is performed as need to avoid variable capture.
subst :: Name -> Term -> Term -> Term
subst x v e = case e of
  App e1 e2                             -> App (rec e1) (rec e2)
  Var y | x == y                        -> v
        | otherwise                     -> e
  Lambda y ty e2 | x == y               -> e
                 | y `notElem` freeVs v -> Lambda y ty (rec e2)
                 | otherwise            -> let newY = until (`notElem` freeVs v) fresh y
                                            in Lambda newY ty (rec (subst y (Var newY) e2))
  Seq t1 t2                             -> Seq (rec t1) (rec t2)
  If t1 t2 t3                           -> If (rec t1) (rec t2) (rec t3)
  Succ t                                -> Succ (rec t)
  Pred t                                -> Pred (rec t)
  IsZero t                              -> IsZero (rec t)
  Ref t                                 -> Ref (rec t)
  Deref t                               -> Deref (rec t)
  Assign t1 t2                          -> Assign (rec t1) (rec t2)
  t                                     -> t
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
  Seq t1 t2       -> freeVs t1 ++ freeVs t2
  Loc _           -> []
  Unit            -> []
  Tru             -> []
  Fls             -> []
  Zero            -> []
  Closure {}      -> []

fresh :: Name -> Name
fresh a = "_" ++ a

-----
peanoToDec :: Term -> Integer
peanoToDec Zero     = 0
peanoToDec (Succ n) = succ (peanoToDec n)
peanoToDec t        = error $ "internal error: can't show term as number: " ++ show t

instance Pretty Term where
  pretty = \case
    App e1 e2                      -> p e1 <+> p e2
    Lambda x t e                   -> parens (mconcat ["\\", pretty x, ":", pretty t, ". ", pretty e])
    Closure env x t                -> parens (mconcat ["Closure ", pretty env, " \\", pretty x, ". ", pretty t])
    Var x                          -> pretty x
    Unit                           -> "unit"
    Seq t1 t2                      -> pretty t1 <> ";" <+> pretty t2
    Ref t                          -> "ref" <+> p t
    Deref t                        -> "!"   <>  p t
    Assign t1 t2                   -> hsep [p t1, ":=", pretty t2]
    Loc l                          -> "Loc" <+> pretty l
    Tru                            -> "true"
    Fls                            -> "false"
    If t1 t2 t3                    -> hsep ["if", pretty t1, "then", pretty t2, "else", p t3]
    Zero                           -> "0"
    Succ t | isNumVal t            -> pretty (peanoToDec (Succ t))
    Succ t | otherwise             -> "succ"   <+> p t
    Pred t                         -> "pred"   <+> p t
    IsZero t                       -> "iszero" <+> p t
    where p = parenIf $ \case App    {}                 -> True
                              If     {}                 -> True
                              Succ t | not (isNumVal t) -> True
                              Pred   {}                 -> True
                              IsZero {}                 -> True
                              Ref    {}                 -> True
                              Deref  {}                 -> True
                              Assign {}                 -> True
                              Loc    {}                 -> True
                              _                         -> False

instance Pretty Type where
  pretty = \case
    TyBool      -> "Bool"
    TyNat       -> "Nat"
    TyUnit      -> "Unit"
    TyRef t     -> "Ref" <+> p t
    TyFun t1 t2 -> hsep [p t1, "->", pretty t2]
    TyVar name  -> pretty name
    where p = parenIf $ \case TyFun {} -> True
                              TyRef {} -> True
                              _        -> False

parenIf :: Pretty a => (a -> Bool) -> a -> Doc b
parenIf f t = (if f t then parens else id) (pretty t)


instance Pretty (Store Location) where
  pretty m = "Store:" <+> pretty (Map.toList m)

instance Pretty NameEnv where
  pretty m = "NameEnv:" <+> align (hvsep (fmap pretty (Map.toList m)))
    where hvsep = concatWith (\x y -> x <> hardline <> y)

