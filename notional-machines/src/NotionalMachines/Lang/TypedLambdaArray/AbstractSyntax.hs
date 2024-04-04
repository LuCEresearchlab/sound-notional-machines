{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module NotionalMachines.Lang.TypedLambdaArray.AbstractSyntax (
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

  Name,
  NameEnv,
  StateRacket(..),

  isValue,
  isNumVal,

  peanoToDec,

  redex,

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

import Prettyprinter (Doc, Pretty, align, comma, concatWith, encloseSep, hardline, hsep, lbrace,
                      parens, pretty, rbrace, (<+>), brackets)

import NotionalMachines.Lang.Error     (Error (..), mismatch, typeOfEq)
import NotionalMachines.Meta.Steppable (SteppableM, evalM, stepM)
import NotionalMachines.Util.Util      (mapFirstM, maybeAt, maybeToEither, stateToStateT, nextKey)

--------------------
-- Simply-Typed Lambda Calculus
-- + Unit
-- + Sequence
-- + References
-- + Arrays
-- + Booleans and Arithmetic Expressions
--------------------
data Type = TyFun Type Type
          | TyUnit
          | TyRef Type
          | TyArray Type
          | TyNull
          | TyBool
          | TyNat
          | TyVar Name
          | TyTuple [Type]
  deriving (Eq, Read, Show)

type TypCtx = [(Name, Type)]

-- type TyStore = Map Location Type

data Term -- Lambdas
          = Var Name
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
          -- Arrays
          | ArrayAlloc Type Term
          | ArrayAccess Term Term
          -- Null
          | Null
          -- Compound data
          | Tuple [Term]
          | Proj Integer Term
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
nextLocation = nextKey

deref :: Location -> StateT (Store Location) (Either Error) Term
deref l = (lift . maybeToEither er . Map.lookup l) =<< get
  where er = RuntimeError $ "address not found: " ++ show l

assign :: Location -> Term -> StateT (Store Location) (Either Error) Term
assign l v = deref l *> withStateT (Map.adjust (const v) l) (return Unit) -- try to find first

--- Array Manipulation ---
-- | alloc @size@ times with default values and return the first location.
-- Array of size Zero and of type TyFun, TyTuple, TyVar, TyNull are not allowed.
allocArray :: Type -> Term -> StateT (Store Location) (Either Error) Location
allocArray ty size = do newLoc <- fmap nextLocation get
                        v <- lift $ defaultVal ty
                        let locs = [newLoc..newLoc + fromIntegral (peanoToDec size) - 1]
                        if length locs == 0
                          then lift $ Left (RuntimeError "can't allocate array of size 0")
                          else withStateT (Map.union (Map.fromList (fmap (,v) locs))) (return newLoc)
  where defaultVal = \case
          TyNat     -> Right Zero
          TyBool    -> Right Fls
          TyUnit    -> Right Unit
          TyArray _ -> Right Null
          TyRef _   -> Right Null
          TyFun _ _ -> Left (RuntimeError "can't allocate array of functions")
          TyTuple _ -> Left (RuntimeError "can't allocate array of tuples")
          TyVar _   -> Left (RuntimeError "how did we get here?")
          TyNull    -> Left (RuntimeError "how did we get here?")

-- | Dereferences the @i@-th location of the array stored at @l@.
-- Equivalent to deref (l + i).
derefArray :: Location -> Term -> StateT (Store Location) (Either Error) Term
derefArray l i = deref (l + fromIntegral (peanoToDec i))

-- | Assigns @v@ to the @i@-th location of the array stored at @l@.
-- Equivalent to assign (l + i) v.
assignArray :: Location -> Term -> Term -> StateT (Store Location) (Either Error) Term
assignArray l i = assign (l + fromIntegral (peanoToDec i))
-------------------

isValue :: Term -> Bool
isValue = \case
  Lambda {} -> True
  Unit      -> True
  Loc {}    -> True
  Null      -> True
  Tru       -> True
  Fls       -> True
  Tuple ts  -> all isValue ts
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
  Assign (ArrayAccess t1 t2) t3 -> do typ1 <- rec t1
                                      typ2 <- rec t2
                                      case typ1 of
                                        TyArray typ11 ->
                                          case typ2 of
                                            TyNat -> typeOfEq' t3 typ11 TyUnit -- T-ArrayAssign
                                            _     -> mismatch' TyNat typ2 t2
                                        _             -> mismatch' (TyArray (TyVar "t")) typ1 t1
  Assign t1 t2    -> do typ1 <- rec t1
                        case typ1 of
                          TyRef typ11 -> typeOfEq' t2 typ11 TyUnit      -- T-Assign
                          _           -> mismatch' (TyRef (TyVar "t")) typ1 t1
  -- Arrays
  ArrayAlloc typ1 t -> do typ2 <- rec t
                          case typ2 of
                            TyNat -> return (TyArray typ1)              -- T-ArrayAlloc
                            _     -> mismatch' TyNat typ2 t
  ArrayAccess t1 t2 -> do typ1 <- rec t1
                          case typ1 of
                            TyArray typ11 -> typeOfEq' t2 TyNat typ11   -- T-ArrayAccess
                            _             -> mismatch' (TyArray (TyVar "t")) typ1 t1
  -- Null
  Null            -> return TyNull                                      -- T-Null
  -- Compound data
  Tuple ts        -> TyTuple <$> mapM rec ts                            -- T-Tuple
  Proj i t        -> do typ1 <- rec t
                        let expected = TyTuple (replicate (fromIntegral i) (TyVar "t"))
                        let err = mismatch' expected typ1 t
                        case typ1 of
                          TyTuple typs -> maybe err Right (maybeAt typs i)
                          _            -> err
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
  Closure {}      -> Left (TypeError "Closures can't be constructed from source code")
  Loc {}          -> Left (TypeError "Locations can't be constructed from source code")
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
  App t1 t2 | not (isValue' t1)    -> (\t1' -> App t1' t2 ) <$> rec t1               -- E-App1
  App v1 t2 | not (isValue' t2)    -> (\t2' -> App v1  t2') <$> rec t2               -- E-App2
  -- Here substitution was replacedy explicit naming environment management
  -- Lambdas are turned into Closure which capture the environment.
  Lambda name _ t                  -> (\(env, _) -> Closure env name t) <$> get
  App (Closure env name t1) t2     -> withStateT (first (const (Map.insert name t2 env))) (return t1)
  v@(Var name)                     -> fromMaybe v . Map.lookup name . fst <$> get
  -- Sequence
  Seq Unit t2                      -> return t2                                      -- E-NextSeq
  Seq t1   t2                      -> (\t1' -> Seq t1' t2) <$> rec t1                -- E-Seq
  -- References
  Ref v | isValue' v               -> augmentState $ stateToStateT $ Loc <$> alloc v -- E-RefV
  Ref t | otherwise                -> Ref   <$> rec t                                -- E-Ref
  Deref (Loc l)                    -> augmentState $ deref l                         -- E-DerefLoc
  Deref t                          -> Deref <$> rec t                                -- E-Deref
  Assign (Loc l) v | isValue' v    -> augmentState $ assign l v                      -- E-Assign
  Assign t1 t2 | not (isValue' t1) -> (\t1' -> Assign t1' t2 ) <$> rec t1            -- E-Assign1
  Assign v1 t2 | otherwise         -> (\t2' -> Assign v1  t2') <$> rec t2            -- E-Assign2
  -- Compound data
  Tuple ts                         -> Tuple <$> mapFirstM (not . isValue') rec ts    -- E-Tuple
  Proj i t@(Tuple vs) | isValue' t -> proj i vs                                      -- E-ProjTuple
  Proj i t                         -> Proj i <$> rec t                               -- E-Proj
  -- Booleans + Arith
  If Tru t2 _                      -> return t2                                      -- E-IfTrue
  If Fls _  t3                     -> return t3                                      -- E-IfFalse
  If t1  t2 t3                     -> (\t1' -> If t1' t2 t3)   <$> rec t1            -- E-If
  Succ t                           -> Succ   <$> rec t                               -- E-Succ
  Pred Zero                        -> return Zero                                    -- E-PredZero
  Pred (Succ v) | isNumVal v       -> return v                                       -- E-PredSucc
  Pred t                           -> Pred   <$> rec t                               -- E-Pred
  IsZero Zero                      -> return Tru                                     -- E-IsZeroZero
  IsZero (Succ v) | isNumVal v     -> return Fls                                     -- E-IsZeroSucc
  IsZero t                         -> IsZero <$> rec t                               -- E-IsZero
  t                                -> return t
  where rec = stepAlaWadler

-- | Generic Proj. The error below is captured before by the type system so it shouldn't happen.
proj :: Integer -> [a] -> StateT s (Either Error) a
proj i vs = lift $ maybeToEither er (maybeAt vs i)
  where er = RuntimeError ("invalid index " ++ show i)
----------------------
----------------------

----------------------
-- step ala racket
----------------------
data StateRacket = StateRacket NameEnv (Store Location)
  deriving (Eq, Show)

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
  App t1 t2 | not (isValue t1)    -> (\t1' -> App t1' t2 ) <$> rec t1                     -- E-App1
  App v1 t2 | not (isValue t2)    -> (\t2' -> App v1  t2') <$> rec t2                     -- E-App2
  App (Lambda name _ t1) t2       -> do StateRacket env s <- get
                                        let newName = until (`Map.notMember` env) fresh name
                                        put $ StateRacket (Map.insert newName t2 env) s
                                        return (subst name (Var newName) t1)
  v@(Var name)                    -> (\(StateRacket env _) -> fromMaybe v (Map.lookup name env)) <$> get
  -- Sequence
  Seq Unit t2                     -> return t2                                            -- E-NextSeq
  Seq t1   t2                     -> (\t1' -> Seq t1' t2) <$> rec t1                      -- E-Seq
  -- References
  Ref v | isValue v               -> storeToStateRacket $ stateToStateT $ Loc <$> alloc v -- E-RefV
  Ref t | otherwise               -> Ref   <$> rec t                                      -- E-Ref
  Deref (Loc l)                   -> storeToStateRacket $ deref l                         -- E-DerefLoc
  Deref t                         -> Deref <$> rec t                                      -- E-Deref
  Assign (Loc l) v | isValue v    -> storeToStateRacket $ assign l v                      -- E-Assign
  Assign t1 t2 | not (isValue t1) -> (\t1' -> Assign t1' t2 ) <$> rec t1                  -- E-Assign1
  Assign v1 t2 | otherwise        -> (\t2' -> Assign v1  t2') <$> rec t2                  -- E-Assign2
  -- Compound data
  Tuple ts                        -> Tuple <$> mapFirstM (not . isValue) rec ts           -- E-Tuple
  Proj i t@(Tuple vs) | isValue t -> proj i vs                                            -- E-ProjTuple
  Proj i t                        -> Proj i <$> rec t                                     -- E-Proj
  -- Booleans + Arith
  If Tru t2 _                     -> return t2                                            -- E-IfTrue
  If Fls _  t3                    -> return t3                                            -- E-IfFalse
  If t1  t2 t3                    -> (\t1' -> If t1' t2 t3)   <$> rec t1                  -- E-If
  Succ t                          -> Succ   <$> rec t                                     -- E-Succ
  Pred Zero                       -> return Zero                                          -- E-PredZero
  Pred (Succ v) | isNumVal v      -> return v                                             -- E-PredSucc
  Pred t                          -> Pred   <$> rec t                                     -- E-Pred
  IsZero Zero                     -> return Tru                                           -- E-IsZeroZero
  IsZero (Succ v) | isNumVal v    -> return Fls                                           -- E-IsZeroSucc
  IsZero t                        -> IsZero <$> rec t                                     -- E-IsZero
  t                               -> return t
  where rec = stepAlaRacket

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
  App t1 t2 | not (isValue t1)    -> (\t1' -> App t1' t2 ) <$> rec t1           -- E-App1
  App v1 t2 | not (isValue t2)    -> (\t2' -> App v1  t2') <$> rec t2           -- E-App2
  App (Lambda name _ t1) t2       -> return $ subst name t2 t1                  -- E-AppAbs
  -- Sequence
  Seq Unit t2                     -> return t2                                  -- E-NextSeq
  Seq t1   t2                     -> (\t1' -> Seq t1' t2) <$> rec t1            -- E-Seq
  -- References
  Ref v | isValue v               -> stateToStateT $ Loc <$> alloc v            -- E-RefV
  Ref t | otherwise               -> Ref   <$> rec t                            -- E-Ref
  Deref (Loc l)                   -> deref l                                    -- E-DerefLoc
  Deref Null                      -> err "can't dereference null"               -- E-DerefNull
  Deref t                         -> Deref <$> rec t                            -- E-Deref
  Assign (Loc l) v | isValue v    -> assign l v                                 -- E-RefAssign
  Assign Null _                   -> err "can't assign to null"                 -- E-AssignNull
  -- Arrays
  ArrayAlloc ty i | isValue i     -> Loc <$> allocArray ty i    -- E-ArrayAlloc1
  ArrayAlloc ty t | otherwise     -> ArrayAlloc ty <$> rec t                    -- E-ArrayAlloc2
  Assign (ArrayAccess (Loc l) i) v
    | isValue i && isValue v      -> assignArray l i v                          -- E-ArrayAssign1
  Assign a@(ArrayAccess (Loc _) i) t
    | isValue i && not (isValue t) -> Assign a <$> rec t                        -- E-ArrayAssign2
  Assign (ArrayAccess Null _) _   -> err "can't dereference null"               -- E-ArrayAssignNull
  Assign t1 t2 | not (isValue t1) -> (\t1' -> Assign t1' t2 ) <$> rec t1        -- E-Assign1
  Assign v1 t2 | otherwise        -> (\t2' -> Assign v1  t2') <$> rec t2        -- E-Assign2
  ArrayAccess (Loc l) i | isValue i -> derefArray l i                          -- E-ArrayAccess1
  ArrayAccess Null _               -> err "can't dereference null"             -- E-ArrayAccessNull
  ArrayAccess t1 t2 | not (isValue t1) -> (\t1' -> ArrayAccess t1' t2) <$> rec t1 -- E-ArrayAccess2
  ArrayAccess v1 t2 | otherwise    -> (\t2' -> ArrayAccess v1  t2') <$> rec t2 -- E-ArrayAccess3
  -- Compound data
  Tuple ts                        -> Tuple <$> mapFirstM (not . isValue) rec ts -- E-Tuple
  Proj i t@(Tuple vs) | isValue t -> proj i vs                                  -- E-ProjTuple
  Proj i t                        -> Proj i <$> rec t                           -- E-Proj
  -- Booleans + Arith
  If Tru t2 _                     -> return t2                                  -- E-IfTrue
  If Fls _  t3                    -> return t3                                  -- E-IfFalse
  If t1  t2 t3                    -> (\t1' -> If t1' t2 t3)   <$> rec t1        -- E-If
  Succ t                          -> Succ   <$> rec t                           -- E-Succ
  Pred Zero                       -> return Zero                                -- E-PredZero
  Pred (Succ v) | isNumVal v      -> return v                                   -- E-PredSucc
  Pred t                          -> Pred   <$> rec t                           -- E-Pred
  IsZero Zero                     -> return Tru                                 -- E-IsZeroZero
  IsZero (Succ v) | isNumVal v    -> return Fls                                 -- E-IsZeroSucc
  IsZero t                        -> IsZero <$> rec t                           -- E-IsZero
  t                               -> return t
  where rec = step'
        err = lift . Left . RuntimeError

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
  ArrayAlloc ty t2                      -> ArrayAlloc ty (rec t2)
  ArrayAccess t1 t2                     -> ArrayAccess (rec t1) (rec t2)
  Tuple ts                              -> Tuple (map rec ts)
  Proj i t                              -> Proj i (rec t)
  t                                     -> t
  where rec = subst x v

freeVs :: Term -> [Name]
freeVs = \case
  Var name          -> [name]
  Lambda name _ t   -> freeVs t \\ [name]
  App t1 t2         -> freeVs t1 ++ freeVs t2
  If t1 t2 t3       -> freeVs t1 ++ freeVs t2 ++ freeVs t3
  Succ t            -> freeVs t
  Pred t            -> freeVs t
  IsZero t          -> freeVs t
  Ref t             -> freeVs t
  Deref t           -> freeVs t
  Assign t1 t2      -> freeVs t1 ++ freeVs t2
  ArrayAlloc _ t    -> freeVs t
  ArrayAccess t1 t2 -> freeVs t1 ++ freeVs t2
  Seq t1 t2         -> freeVs t1 ++ freeVs t2
  Tuple ts          -> concatMap freeVs ts
  Proj _ t          -> freeVs t
  Loc _             -> []
  Null              -> []
  Unit              -> []
  Tru               -> []
  Fls               -> []
  Zero              -> []
  Closure {}        -> []

fresh :: Name -> Name
fresh a = "_" ++ a

-----
peanoToDec :: Term -> Integer
peanoToDec Zero     = 0
peanoToDec (Succ n) = succ (peanoToDec n)
peanoToDec t        = error $ "internal error: can't show term as number: " ++ show t

instance Pretty Term where
  pretty = \case
    App e1 e2           -> p e1 <+> p e2
    Lambda x t e        -> parens (mconcat ["\\", pretty x, ":", pretty t, ". ", pretty e])
    Closure env x t     -> parens (mconcat ["Closure ", pretty env, " \\", pretty x, ". ", pretty t])
    Var x               -> pretty x
    Unit                -> "unit"
    Seq t1 t2           -> pretty t1 <> ";" <+> pretty t2
    Ref t               -> "ref" <+> p t
    Deref t             -> "!"   <>  p t
    Assign t1 t2        -> hsep [p t1, ":=", pretty t2]
    Loc l               -> "Loc" <+> pretty l
    ArrayAlloc ty t2    -> "array" <+> pretty ty <> brackets (pretty t2)
    ArrayAccess t1 t2   -> p t1 <+> "|" <+> p t2
    Null                -> "null"
    Tuple ts            -> encloseSep lbrace rbrace comma (map pretty ts)
    Proj i t            -> p t <> "." <> pretty i
    Tru                 -> "true"
    Fls                 -> "false"
    If t1 t2 t3         -> hsep ["if", pretty t1, "then", pretty t2, "else", p t3]
    Zero                -> "0"
    Succ t | isNumVal t -> pretty (peanoToDec (Succ t))
    Succ t | otherwise  -> "succ"   <+> p t
    Pred t              -> "pred"   <+> p t
    IsZero t            -> "iszero" <+> p t
    where p = parenIf $ \case App    {}                 -> True
                              If     {}                 -> True
                              Succ t | not (isNumVal t) -> True
                              Pred   {}                 -> True
                              IsZero {}                 -> True
                              Ref    {}                 -> True
                              Deref  {}                 -> True
                              Assign {}                 -> True
                              ArrayAlloc {}             -> True
                              ArrayAccess {}            -> True
                              Loc    {}                 -> True
                              Proj   {}                 -> True
                              _                         -> False

instance Pretty Type where
  pretty = \case
    TyBool      -> "Bool"
    TyNat       -> "Nat"
    TyUnit      -> "Unit"
    TyRef t     -> "Ref" <+> p t
    TyArray t   -> "Array" <+> p t
    TyNull      -> "NullType"
    TyFun t1 t2 -> hsep [p t1, "->", pretty t2]
    TyTuple ts  -> encloseSep lbrace rbrace comma (map pretty ts)
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



----------------------
-- Other utilities

redex :: Term -> Term
redex e = case e of
  -- Lambdas
  App t1 _  | not (isValue t1)         -> rec t1
  App _  t2 | not (isValue t2)         -> rec t2
  App (Lambda _ _ _) _            -> e
  -- Sequence
  Seq Unit _                          -> e
  Seq t1   _                          -> rec t1
  -- References
  Ref v | isValue v                    -> e
  Ref t | otherwise                    -> rec t
  Deref (Loc _)                        -> e
  Deref Null                           -> e -- err "can't dereference null"
  Deref t                              -> rec t
  Assign (Loc _) v | isValue v         -> e
  Assign Null _                        -> e -- err "can't assign to null"
  -- Arrays
  ArrayAlloc _ i | isValue i          -> e
  ArrayAlloc _ t | otherwise          -> rec t
  Assign (ArrayAccess (Loc _) i) v
    | isValue i && isValue v           -> e
  Assign (ArrayAccess (Loc _) i) t
    | isValue i && not (isValue t)     -> rec t
  Assign (ArrayAccess Null _) _        -> e -- err "can't dereference null"
  Assign t1 _  | not (isValue t1)      -> rec t1
  Assign _  t2 | otherwise             -> rec t2
  ArrayAccess (Loc _) i | isValue i    -> e
  ArrayAccess Null _                   -> e -- err "can't dereference null"
  ArrayAccess t1 _  | not (isValue t1) -> rec t1
  ArrayAccess _  t2 | otherwise        -> rec t2
  -- Compound data
  Tuple ts | all isValue ts            -> e
  Tuple ts | otherwise                 -> rec (head (filter (not . isValue) ts))
  Proj _ t@(Tuple _) | isValue t      -> e
  Proj _ t                             -> rec t
  -- Booleans + Arith
  If Tru _ _                          -> e
  If Fls _ _                         -> e
  If t1  _ _                         -> rec t1
  Succ t                               -> rec t
  Pred Zero                            -> e
  Pred (Succ v) | isNumVal v           -> e
  Pred t                               -> rec t
  IsZero Zero                          -> e
  IsZero (Succ v) | isNumVal v         -> e
  IsZero t                             -> rec t
  _                                    -> e
  where rec = redex

