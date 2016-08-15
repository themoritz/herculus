{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Lib.Compiler.Typechecker
  ( runInfer
  , Scheme (..)
  , Type (..)
  ) where

import           Control.Lens                   hiding (Context)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List                      (intercalate)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text, pack)

import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker.Types
import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

--

prelude :: Context
prelude = Context $ Map.fromList
  [ ( "zero"
    , Forall [] $ TNullary TNumber
    )
  , ( "double"
    , Forall [] $ TNullary TNumber `TArr` TNullary TNumber
    )
  , ( "sum"
    , Forall [] $ (TUnary TList $ TNullary TNumber) `TArr` TNullary TNumber
    )
  , ( "map"
    , Forall [TV "_a", TV "_b"] $
        (TVar (TV "_a") `TArr` (TVar (TV "_b"))) `TArr`
        (TUnary TList (TVar (TV "_a")) `TArr` TUnary TList (TVar (TV "_b")))
    )
  , ( "filter"
    , Forall [TV "_a"] $
        (TVar (TV "_a") `TArr` (TNullary TBool)) `TArr`
        (TUnary TList (TVar (TV "_a")) `TArr` TUnary TList (TVar (TV "_a")))
    )
  , ( "find"
    , Forall [TV "_a"] $
        (TVar (TV "_a") `TArr` (TNullary TBool)) `TArr`
        (TUnary TList (TVar (TV "_a")) `TArr` TUnary TMaybe (TVar (TV "_a")))
    )
  ]

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
  apply s t@(TVar a)      = Map.findWithDefault t a s
  apply _ (TNullary s)    = TNullary s
  apply s (TUnary n t)    = TUnary n (apply s t)
  apply s (TArr t1 t2)    = TArr (apply s t1) (apply s t2)
  apply s (TRecord r)     = TRecord (apply s r)
  apply s (TRow name t r) = TRow name (apply s t) (apply s r)
  apply s (TNoRow)        = TNoRow

  ftv (TVar a)     = Set.singleton a
  ftv (TNullary _) = Set.empty
  ftv (TUnary _ t) = ftv t
  ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TRecord r)  = ftv r
  ftv (TRow _ t r) = ftv t `Set.union` ftv r
  ftv (TNoRow)     = Set.empty

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
newInferState = InferState 0 prelude

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
      [ (tcond, TNullary TBool)
      , (te1, te2)
      ]
    pure $ If cond' e1' e2' ::: apply s te2
  Var x -> do
    t <- lookupEnv x
    pure $ Var x ::: t
  Lit l -> do
    let t = case l of
          LNumber _ -> TNumber
          LBool _   -> TBool
          LString _ -> TString
    pure $ Lit l ::: (TNullary t)
  Binop op l r -> do
    l' ::: tl <- infer l
    r' ::: tr <- infer r
    tres <- fresh
    let num = TNullary TNumber
        is     = tl  `TArr` (tr  `TArr` tres)
        should = num `TArr` (num `TArr` num)
    s <- unify [ (is, should) ]
    pure $ Binop op l' r' ::: apply s tres
  PrjRecord e name -> do
    e' ::: te <- infer e
    tres <- fresh
    trow <- fresh
    s <- unify [ (te, TRecord $ TRow name tres trow) ]
    pure $ PrjRecord e' name ::: apply s tres
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
        pure $ ColumnOfTableRef tblId colId ::: TUnary TList t
  TableRef tblRef -> do
    lift (resolveTableRef tblRef) >>= \case
      Nothing -> throwError $ pack $ "table not found: " <> show tblRef
      Just (i, r) -> do
        let toRow [] = TNoRow
            toRow ((c, t):rest) = TRow c t (toRow rest)
        pure $ TableRef i ::: TUnary TList (TRecord $ toRow $ Map.toList r)

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
    unifyOne (TNullary a) (TNullary b) | a == b = pure nullSubst
    unifyOne (TUnary n l) (TUnary m r) | n == m = unifyOne l r
    unifyOne (TArr l r) (TArr l' r') = do
      s1 <- unifyOne l l'
      s2 <- unifyOne (apply s1 r) (apply s1 r')
      pure (s2 `compose` s1)
    unifyOne (TRecord l) (TRecord r) = unifyOne l r
    unifyOne (TRow n1 t1 r1) (TRow n2 t2 r2)
      | n1 == n2 =  do s1 <- unifyOne t1 t2
                       s2 <- unifyOne (apply s1 r1) (apply s1 r2)
                       pure (s2 `compose` s1)
      | otherwise = do a <- fresh
                       s1 <- unifyOne r1 (TRow n2 t2 a)
                       s2 <- unifyOne (apply s1 r2) (TRow n1 (apply s1 t1) (apply s1 a))
                       pure (s2 `compose` s1)
    unifyOne t1 t2 = throwError $ pack $ "type mismatch: " <> show t1 <> ", " <> show t2

    bind :: TVar -> Type -> InferT m Subst
    bind a t | t == TVar a     = pure nullSubst
             | occursCheck a t = throwError "occurs check: infinite type"
             | otherwise       = pure $ Map.singleton a t

    occursCheck :: Substitutable a => TVar -> a -> Bool
    occursCheck a t = a `Set.member` ftv t
