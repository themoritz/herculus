{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler.Typechecker
  ( runInfer
  , Scheme (..)
  , Type (..)
  ) where

import           Control.Lens              hiding (Context)
import           Control.Monad.Except
import           Control.Monad.State.Class
import           Control.Monad.Trans.State (StateT, evalStateT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Reader.Class

import           Data.List                 (intercalate)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text, pack)

import           Lib.Types
import           Lib.Model
import           Lib.Model.Types
import           Lib.Model.Column hiding ((:::))
import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker.Types

--

newtype Context = Context (Map Name Scheme)
  deriving (Show)

emptyContext :: Context
emptyContext = Context Map.empty

extend :: (Name, Scheme) -> Context -> Context
extend (x, s) (Context env) = Context $ Map.insert x s env

remove :: Name -> Context -> Context
remove x (Context env) = Context $ Map.delete x env

getScheme :: Name -> Context -> Maybe Scheme
getScheme x (Context env) = Map.lookup x env

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
  apply _ (TBase s)    = TBase s
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
  apply s (TList t)    = TList (apply s t)

  ftv (TVar a)     = Set.singleton a
  ftv (TBase _)    = Set.empty
  ftv (TArr t1 t2) = Set.union (ftv t1) (ftv t2)
  ftv (TList t)    = ftv t

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

instance Substitutable Context where
  apply s (Context env) = Context $ Map.map (apply s) env
  ftv (Context env) = ftv $ Map.elems env

--

data InferState = InferState
  { _inferCount   :: Int
  , _inferContext :: Context
  }

makeLenses ''InferState

newInferState :: InferState
newInferState = InferState 0 emptyContext

type TypeError = Text

newtype InferT m a = InferT
  { unInferT :: ReaderT (Id Table) (StateT InferState (ExceptT TypeError m)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Id Table)
             , MonadError TypeError
             , MonadState InferState
             )

instance MonadTrans InferT where
  lift = InferT . lift . lift . lift

runInfer :: MonadTypecheck m => Id Table -> Expr Ref -> m (Either TypeError TypedExpr)
runInfer tblId expr =
  runExceptT $ evalStateT (runReaderT (unInferT $ infer expr) tblId) newInferState

--

generalize :: Monad m => Type -> InferT m Scheme
generalize t = do
  env <- use inferContext
  let as = Set.toList $ ftv t `Set.difference` ftv env
  pure $ Forall as t

instantiate :: Monad m => Scheme -> InferT m Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let subst = Map.fromList $ zip as as'
  pure $ apply subst t

fresh :: Monad m => InferT m Type
fresh = do
    c <- inferCount <+= 1
    return $ TVar $ TV (pack $ letters !! c)
  where
    letters :: [String]
    letters = [1..] >>= flip replicateM ['a'..'z']

inEnv :: Monad m => (Name, Scheme) -> InferT m a -> InferT m (a, Scheme)
inEnv (x, sc) m = do
  inferContext %= extend (x, sc) . remove x
  res <- m
  context <- use inferContext
  let (Just sc') = getScheme x context
  inferContext %= remove x
  pure (res, sc')

lookupEnv :: Monad m => Name -> InferT m Type
lookupEnv x = do
  env <- use inferContext
  case getScheme x env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ pack $ "not in scope: " <> x

infer :: MonadTypecheck m => Expr Ref -> InferT m TypedExpr
infer expr = case expr of
  Lam x e -> do
    tv <- fresh
    (e' ::: t, Forall [] tv') <- inEnv (x, Forall [] tv) (infer e)
    pure $ Lam x e' ::: TArr tv' t
  App f arg -> do
    f' ::: tf <- infer f
    arg' ::: targ <- infer arg
    tres <- fresh
    s <- unify [(tf, (TArr targ tres))]
    pure $ App f' arg' ::: apply s tres
  Let x e rest -> do
    e' ::: te <- infer e
    scheme <- generalize te
    (rest' ::: trest, _) <- inEnv (x, scheme) (infer rest)
    pure $ Let x e' rest' ::: trest
  If cond e1 e2 -> do
    cond' ::: tcond <- infer cond
    e1' ::: te1 <- infer e1
    e2' ::: te2 <- infer e2
    s <- unify
      [ (tcond, typeBool)
      , (te1, te2)
      ]
    pure $ If cond' e1' e2' ::: apply s te2
  Var x -> do
    t <- lookupEnv x
    pure $ Var x ::: t
  Lit l -> do
    let t = case l of
          LNumber _ -> typeNumber
          LBool _   -> typeBool
          LString _ -> typeString
    pure $ Lit l ::: t
  Binop op l r -> do
    l' ::: tl <- infer l
    r' ::: tr <- infer r
    tres <- fresh
    let is     = tl         `TArr` (tr         `TArr` tres)
        should = typeNumber `TArr` (typeNumber `TArr` typeNumber)
    s <- unify [ (is, should) ]
    pure $ Binop op l' r' ::: apply s tres
  PrjRecord e name -> do
    e' ::: te <- infer e
    tres <- fresh
    -- TODO: unify
    pure $ PrjRecord e' name ::: tres
  ColumnRef colRef -> do
    tblId <- ask
    lift (resolveColumnRef tblId colRef) >>= \case
      Nothing -> throwError $ pack $ "column not found: " <> show colRef
      Just (Entity i col) -> do
        let t = typeOfDataType $ columnDataType col
        pure $ ColumnRef i ::: t
  ColumnOfTableRef tblRef colRef -> do
    lift (resolveColumnOfTableRef tblRef colRef) >>= \case
      Nothing -> throwError $ pack $ "column not found: " <>
                                     show colRef <> " on table " <>
                                     show tblRef
      Just (tblId, Entity colId col) -> do
        let t = typeOfDataType $ columnDataType col
        pure $ ColumnOfTableRef tblId colId ::: TList t
  TableRef tblRef -> do
    lift (resolveTableRef tblRef) >>= \case
      Nothing -> throwError $ pack $ "table not found: " <> show tblRef
      Just (i, r) -> pure $ TableRef i ::: TList (TRecord r)

--

unify :: forall m. Monad m => [(Type, Type)] -> InferT m Subst
unify cs = do
    s <- unify' cs
    inferContext %= apply s
    pure s
  where
    unify' :: [(Type, Type)] -> InferT m Subst
    unify' [] = pure nullSubst
    unify' ((t1, t2):cs) = do
      s1 <- unifyOne t1 t2
      s2 <- unify' (apply s1 cs)
      pure $ s2 `compose` s1

    unifyOne :: Type -> Type -> InferT m Subst
    unifyOne (TVar a) t = bind a t
    unifyOne t (TVar a) = bind a t
    unifyOne (TArr l r) (TArr l' r') = do
      s1 <- unifyOne l l'
      s2 <- unifyOne (apply s1 r) (apply s1 r')
      pure (s2 `compose` s1)
    unifyOne (TBase a) (TBase b) | a == b = pure nullSubst
    unifyOne t1 t2 = throwError $ pack $ "type mismatch: " <> show t1 <> ", " <> show t2

    bind :: TVar -> Type -> InferT m Subst
    bind a t | t == TVar a     = pure nullSubst
             | occursCheck a t = throwError "occurs check: infinite type"
             | otherwise       = pure $ Map.singleton a t

    occursCheck :: Substitutable a => TVar -> a -> Bool
    occursCheck a t = a `Set.member` ftv t
