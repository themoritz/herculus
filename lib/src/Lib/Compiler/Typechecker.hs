{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Compiler.Typechecker
  ( runInfer
  , infer
  , fresh
  , unify
  , inEnv
  , newInferState
  , Scheme (..)
  , Type (..)
  ) where

import           Control.Lens                   hiding (Context, op)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map                       as Map
import           Data.Monoid
import qualified Data.Set                       as Set
import           Data.Text                      (pack)

import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Types
import           Lib.Model.Column
import           Lib.Types

--

prelude :: Context
prelude = Context $ Map.fromList
  [ ( "zero"
    , Forall [] $ TyNullary TyNumber
    )
  , ( "double"
    , Forall [] $ TyNullary TyNumber `TyArr` TyNullary TyNumber
    )
  , ( "sum"
    , Forall [] $ TyUnary TyList (TyNullary TyNumber) `TyArr` TyNullary TyNumber
    )
  , ( "length"
    , Forall [] $ TyUnary TyList (TyVar (TV "_a")) `TyArr` TyNullary TyNumber
    )
  , ( "not"
    , Forall [] $ TyNullary TyBool `TyArr` TyNullary TyBool
    )
  , ( "show"
    , Forall [] $ TyNullary TyNumber `TyArr` TyNullary TyString
    )
  , ( "formatNumber"
    , Forall [] $ TyNullary TyString `TyArr` (TyNullary TyNumber `TyArr` TyNullary TyString)
    )
  , ( "formatTime"
    , Forall [] $ TyNullary TyString `TyArr` (TyNullary TyTime `TyArr` TyNullary TyString)
    )
  , ( "map"
    , Forall [TV "_a", TV "_b"] $
        (TyVar (TV "_a") `TyArr` TyVar (TV "_b")) `TyArr`
        (TyUnary TyList (TyVar (TV "_a")) `TyArr` TyUnary TyList (TyVar (TV "_b")))
    )
  , ( "filter"
    , Forall [TV "_a"] $
        (TyVar (TV "_a") `TyArr` TyNullary TyBool) `TyArr`
        (TyUnary TyList (TyVar (TV "_a")) `TyArr` TyUnary TyList (TyVar (TV "_a")))
    )
  , ( "find"
    , Forall [TV "_a"] $
        (TyVar (TV "_a") `TyArr` TyNullary TyBool) `TyArr`
        (TyUnary TyList (TyVar (TV "_a")) `TyArr` TyUnary TyMaybe (TyVar (TV "_a")))
    )
  ]

newInferState :: InferState
newInferState = InferState 0 prelude

runInfer :: Monad m => TypecheckEnv m -> PExpr -> m (Either TypeError TypedExpr)
runInfer env expr =
  runExceptT $ evalStateT (runReaderT (unInferT $ infer expr) env) newInferState

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
    return $ TyVar $ TV (pack $ letters !! c)
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

infer :: Monad m => PExpr -> InferT m TypedExpr
infer expr = case expr of
  PLam x e -> do
    tv <- fresh
    (e' ::: t, Forall [] tv') <- inEnv (x, Forall [] tv) (infer e)
    pure $ TLam x e' ::: TyArr tv' t
  PApp f arg -> do
    f' ::: tf <- infer f
    arg' ::: targ <- infer arg
    tres <- fresh
    s <- unify [(tf, TyArr targ tres)]
    pure $ TApp f' arg' ::: apply s tres
  PLet x e rest -> do
    e' ::: te <- infer e
    scheme <- generalize te
    (rest' ::: trest, _) <- inEnv (x, scheme) (infer rest)
    pure $ TLet x e' rest' ::: trest
  PIf cond e1 e2 -> do
    cond' ::: tcond <- infer cond
    e1' ::: te1 <- infer e1
    e2' ::: te2 <- infer e2
    s <- unify
      [ (tcond, TyNullary TyBool)
      , (te1, te2)
      ]
    pure $ TIf cond' e1' e2' ::: apply s te2
  PVar x -> do
    t <- lookupEnv x
    pure $ TVar x ::: t
  PLit l -> do
    let t = case l of
          LNumber _ -> TyNumber
          LBool _   -> TyBool
          LString _ -> TyString
    pure $ TLit l ::: TyNullary t
  PBinop op l r -> do
    l' ::: tl <- infer l
    r' ::: tr <- infer r
    tres <- fresh
    let num = TyNullary TyNumber
        tim = TyNullary TyTime
        bol = TyNullary TyBool
        str = TyNullary TyString
        (arg, res) = case op of
          Add       -> (num, num)
          Sub       -> (num, num)
          Mul       -> (num, num)
          Div       -> (num, num)
          Equal     -> (str, bol)
          NotEqual  -> (str, bol)
          LessEq    -> (tim, bol)
          GreaterEq -> (tim, bol)
          Less      -> (tim, bol)
          Greater   -> (tim, bol)
          And       -> (bol, bol)
          Or        -> (bol, bol)
        is     = tl  `TyArr` (tr  `TyArr` tres)
        should = arg `TyArr` (arg `TyArr` res)
    s <- unify [ (is, should) ]
    pure $ TBinop op l' r' ::: apply s tres
  PPrjRecord e name -> do
    e' ::: te <- infer e
    tres <- fresh
    trow <- fresh
    s <- unify [ (te, TyRecord $ TyRow name tres trow) ]
    pure $ TPrjRecord e' name ::: apply s tres
  PColumnRef colRef -> do
    f <- asks envResolveColumnRef
    lift (f colRef) >>= \case
      Nothing -> throwError $ pack $ "column not found: " <> show colRef
      Just (i, dataCol) -> do
        getRows <- asks envGetTableRows
        t <- lift $ typeOfDataType getRows $ _dataColType dataCol
        pure $ TColumnRef i ::: t
  PColumnOfTableRef tblRef colRef -> do
    f <- asks envResolveColumnOfTableRef
    lift (f tblRef colRef) >>= \case
      Nothing -> throwError $ pack $ "column not found: " <>
                                     show colRef <> " on table " <>
                                     show tblRef
      Just (colId, dataCol) -> do
        getRows <- asks envGetTableRows
        t <- lift $ typeOfDataType getRows $ _dataColType dataCol
        pure $ TWholeColumnRef colId ::: TyUnary TyList t
  PTableRef tblRef -> do
    f <- asks envResolveTableRef
    lift (f tblRef) >>= \case
      Nothing -> throwError $ pack $ "table not found: " <> show tblRef
      Just (i, cols) -> do
        getRows <- asks envGetTableRows
        tblRows <- lift $ getRows i
        pure $ TTableRef i (map fst cols) ::: TyUnary TyList (TyRecord tblRows)

--

unify :: forall m. Monad m => [(Type, Type)] -> InferT m Subst
unify cs = do
    s <- unify' cs
    inferContext %= apply s
    pure s
  where
    unify' :: [(Type, Type)] -> InferT m Subst
    unify' [] = pure nullSubst
    unify' ((t1, t2):cs') = do
      s1 <- unifyOne t1 t2
      s2 <- unify' (apply s1 cs')
      pure $ s2 `compose` s1

    unifyOne :: Type -> Type -> InferT m Subst
    unifyOne (TyVar a) t = bind a t
    unifyOne t (TyVar a) = bind a t
    unifyOne (TyNullary a) (TyNullary b) | a == b = pure nullSubst
    unifyOne (TyUnary n l) (TyUnary m r) | n == m = unifyOne l r
    unifyOne (TyArr l r) (TyArr l' r') = do
      s1 <- unifyOne l l'
      s2 <- unifyOne (apply s1 r) (apply s1 r')
      pure (s2 `compose` s1)
    unifyOne (TyRecord l) (TyRecord r) = unifyOne l r
    unifyOne (TyRow n1 t1 r1) (TyRow n2 t2 r2)
      | n1 == n2 =  do s1 <- unifyOne t1 t2
                       s2 <- unifyOne (apply s1 r1) (apply s1 r2)
                       pure (s2 `compose` s1)
      | otherwise = do a <- fresh
                       s1 <- unifyOne r1 (TyRow n2 t2 a)
                       s2 <- unifyOne (apply s1 r2) (TyRow n1 (apply s1 t1) (apply s1 a))
                       pure (s2 `compose` s1)
    unifyOne t1 t2 = throwError $ pack $ "type mismatch: " <> show t1 <> ", " <> show t2

    bind :: TVar -> Type -> InferT m Subst
    bind a t | t == TyVar a    = pure nullSubst
             | occursCheck a t = throwError "occurs check: infinite type"
             | otherwise       = pure $ Map.singleton a t

    occursCheck :: Substitutable a => TVar -> a -> Bool
    occursCheck a t = a `Set.member` ftv t
