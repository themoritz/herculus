{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler.Typechecker
  ( typecheckExpr
  , Scheme (..)
  , Type (..)
  ) where

import Control.Monad.Trans.RWS hiding (get, put)
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad.State.Class
import Control.Monad.Except

import Debug.Trace

import Data.Text (Text, pack)
import Data.Map (Map)
import Data.Set (Set)
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Lib.Compiler.Parser

--

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV a) = a

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVar a) = show a
  show (TCon c) = c
  show (TArr a b) = "(" <> show a <> " -> " <> show b <> ")"

data Scheme = Forall [TVar] Type

instance Show Scheme where
  show (Forall as t) = "forall " <> intercalate " " (map show as) <> ". " <> show t

typeBool :: Type
typeBool = TCon "Bool"

typeInt :: Type
typeInt = TCon "Int"

typeString :: Type
typeString = TCon "String"

--

newtype TypeEnv = TypeEnv (Map Name Scheme)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

extend :: (Name, Scheme) -> TypeEnv -> TypeEnv
extend (x, s) (TypeEnv env) = TypeEnv $ Map.insert x s env

remove :: Name -> TypeEnv -> TypeEnv
remove x (TypeEnv env) = TypeEnv $ Map.delete x env

--

type Subst = Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s2 s1 = Map.map (apply s2) s1 `Map.union` s2

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TVar

instance Substitutable Type where
  apply s t@(TVar a)   = Map.findWithDefault t a s
  apply _ (TCon s)     = TCon s
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)

  ftv (TVar a)     = Set.singleton a
  ftv (TCon _)     = Set.empty
  ftv (TArr t1 t2) = Set.union (ftv t1) (ftv t2)

instance Substitutable Scheme where
  apply s (Forall as t) = let s' = foldr Map.delete s as
                          in Forall as (apply s' t)
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv = foldr (Set.union . ftv) Set.empty

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  apply s (a, b) = (apply s a, apply s b)
  ftv (a, b) = ftv a `Set.union` ftv b

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

generalize :: Type -> Infer Scheme
generalize t = do
  env <- ask
  let as = Set.toList $ ftv t `Set.difference` ftv env
  pure $ Forall as t

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let subst = Map.fromList $ zip as as'
  pure $ apply subst t

--

data InferState = InferState Int

newInferState :: InferState
newInferState = InferState 0

type TypeError = Text

type Infer a = RWST TypeEnv [Constraint] InferState (Either TypeError) a

runInfer :: Expr -> Either TypeError (Type, [Constraint])
runInfer expr = do
  (typ, _, cs) <- runRWST (infer expr)
                             emptyTypeEnv
                             newInferState
  pure (typ, cs)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  (InferState c) <- get
  put $ InferState (c + 1)
  return $ TVar $ TV (letters !! c)

uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope = extend (x, sc) . remove x
  local scope m

lookupEnv :: Name -> Infer Type
lookupEnv x = do
  TypeEnv env <- ask
  case Map.lookup x env of
    Just scheme -> instantiate scheme
    Nothing -> throwError "type not defined in env (not in scope?)"

infer :: Expr -> Infer Type
infer expr = case expr of
  Lam x e -> do
    tv <- fresh
    t <- inEnv (x, Forall [] tv) (infer e)
    pure $ TArr tv t
  App f arg -> do
    tf <- infer f
    traceM "App"
    traceShowM tf
    targ <- infer arg
    tres <- fresh
    traceShowM (TArr targ tres)
    uni tf (TArr targ tres)
    pure tres
  Let x e rest -> do
    te <- infer e
    traceM "Let"
    traceShowM te
    scheme <- generalize te
    trest <- inEnv (x, scheme) (infer rest)
    pure trest
  If cond e1 e2 -> do
    tcond <- infer cond
    te1 <- infer e1
    te2 <- infer e2
    uni tcond typeBool
    uni te1 te2
    pure te2
  Var x -> lookupEnv x
  Lit l -> case l of
    LInt _ -> pure $ typeInt
    LBool _ -> pure $ typeBool
    LString _ -> pure $ typeString
  Binop op l r -> do
    tl <- infer l
    tr <- infer r
    tres <- fresh
    let is     = tl      `TArr` (tr      `TArr` tres)
        should = typeInt `TArr` (typeInt `TArr` typeInt)
    uni is should
    pure tres

--

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

type Solve a = StateT Unifier (Either TypeError) a

runSolve :: [Constraint] -> Type -> Either TypeError Type
runSolve cs typ = do
  traceM "runSolve"
  traceShowM cs
  subst <- evalStateT solver (nullSubst, cs)
  traceShowM typ
  traceShowM subst
  pure $ apply subst typ

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

bind :: TVar -> Type -> Solve Unifier
bind a t | t == TVar a     = pure emptyUnifier
         | occursCheck a t = throwError "occurs check: infinite type"
         | otherwise       = pure (Map.singleton a t, [])

unify :: Type -> Type -> Solve Unifier
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TArr l r) (TArr l' r') = do
  (s1, c1) <- unify l l'
  (s2, c2) <- unify (apply s1 r) (apply s1 r')
  pure (s2 `compose` s1, c1 <> c2)
unify (TCon a) (TCon b) | a == b = pure emptyUnifier
unify t1 t2 = throwError $ pack $ "type mismatch: " <> show t1 <> " and " <> show t2

solver :: Solve Subst
solver = do
  (s, cs) <- get
  case cs of
    [] -> pure s
    ((t1, t2): cs0) -> do
      (s1, c1) <- unify t1 t2
      put (s1 `compose` s, c1 <> apply s1 cs0)
      solver

--

typecheckExpr :: Expr -> Either TypeError Type
typecheckExpr expr = do
  (typ, cs) <- runInfer expr
  runSolve cs typ
