{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lib.Compiler.Typechecker
  ( runInfer
  , infer
  , fresh
  , unify
  , newInferState
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
import qualified Data.UnionFind.IntMap          as UF

import           Lib.Compiler.Typechecker.Prim
import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Types
import           Lib.Model.Column
import           Lib.Types

--

newInferState :: InferState
newInferState = InferState
  { _inferContext = Context Map.empty
  , _inferCount = 0
  , _inferPointSupply = UF.newPointSupply
  -- , _inferTypeClasses = primTypeClasses
  -- , _inferTypeClassDicts = primTypeClassDicts
  }

runInfer :: Monad m => TypecheckEnv m -> PExpr -> m (Either TypeError (TExpr, Type))
runInfer env expr =
  let action = do
        e ::: t <- infer expr
        t' <- pointToType t
        pure (e, t')
  in  runExceptT $ evalStateT (runReaderT (unInferT action) env) newInferState

--

infer :: Monad m => PExpr -> InferT m TypedExpr
infer expr = case expr of
  PLam x e -> do
    tx <- fresh
    e' ::: t <- inLocalContext (x, ForAll [] [] tx) (infer e)
    arrPt <- arr tx t
    pure $ TLam x e' ::: arrPt
  PApp f arg -> do
    f' ::: tf <- infer f
    arg' ::: targ <- infer arg
    tres <- fresh
    arrPt <- arr targ tres
    unify tf arrPt
    pure $ TApp f' arg' ::: tres
  PLet x e rest -> do
    e' ::: te <- infer e
    poly <- generalize te
    rest' ::: trest <- inLocalContext (x, poly) (infer rest)
    pure $ TLet x e' rest' ::: trest
  PIf cond e1 e2 -> do
    cond' ::: tcond <- infer cond
    e1' ::: te1 <- infer e1
    e2' ::: te2 <- infer e2
    ptBool <- mkPoint tyBool
    unify tcond ptBool
    unify te1 te2
    pure $ TIf cond' e1' e2' ::: te1
  PVar x -> do
    poly <- lookupName x
    tx <- instantiate poly
    pure $ TVar x ::: tx
  PLit l -> do
    t <- case l of
      LNumber _ -> mkPoint tyNumber
      LBool _   -> mkPoint tyBool
      LString _ -> mkPoint tyString
    pure $ TLit l ::: t
  -- PPrjRecord e name -> do
  --   e' ::: te <- infer e
  --   tres <- fresh
  --   trow <- fresh
  --   s <- unify [ (te, TyRecord $ TyRow name tres trow) ]
  --   pure $ TPrjRecord e' name ::: tres
  -- PColumnRef colRef -> do
  --   f <- asks envResolveColumnRef
  --   lift (f colRef) >>= \case
  --     Nothing -> throwError $ pack $ "column not found: " <> show colRef
  --     Just (i, dataCol) -> do
  --       getRows <- asks envGetTableRows
  --       t <- lift $ typeOfDataType getRows $ _dataColType dataCol
  --       pure $ TColumnRef i ::: t
  -- PColumnOfTableRef tblRef colRef -> do
  --   f <- asks envResolveColumnOfTableRef
  --   lift (f tblRef colRef) >>= \case
  --     Nothing -> throwError $ pack $ "column not found: " <>
  --                                    show colRef <> " on table " <>
  --                                    show tblRef
  --     Just (colId, dataCol) -> do
  --       getRows <- asks envGetTableRows
  --       t <- lift $ typeOfDataType getRows $ _dataColType dataCol
  --       pure $ TWholeColumnRef colId ::: TyUnary TyList t
  -- PTableRef tblRef -> do
  --   f <- asks envResolveTableRef
  --   lift (f tblRef) >>= \case
  --     Nothing -> throwError $ pack $ "table not found: " <> show tblRef
  --     Just (i, cols) -> do
  --       getRows <- asks envGetTableRows
  --       tblRows <- lift $ getRows i
  --       pure $ TTableRef i (map fst cols) ::: TyUnary TyList (TyRecord tblRows)

generalize :: MonadState InferState m => Point -> m PolyType
generalize pt = do
  context <- use inferContext
  freeTypeVariables <- Set.difference <$> ftv pt <*> ftv context
  pure $ ForAll (Set.toList freeTypeVariables) [] pt

instantiate :: MonadState InferState m => PolyType -> m Point
instantiate (ForAll as _ p) = do
  pool <- mapM (\a -> (a,) <$> fresh) as
  let replace p' = do
        t <- findType p'
        case t of
          TyVar a -> case lookup a pool of
            Nothing -> pure p'
            Just f -> pure f
          TyConst _ -> pure p'
          TyApp l r -> do
            app <- TyApp <$> replace l <*> replace r
            mkPoint app
  replace p

unify :: Monad m => Point -> Point -> InferT m ()
unify a b = do
    ta <- findType a
    tb <- findType b
    case (ta, tb) of
      (TyVar x, _) -> bind x a b
      (_, TyVar x) -> bind x b a
      (TyConst x, TyConst y) | x == y -> pure ()
      (TyApp la ra, TyApp lb rb) -> do
        unify la lb
        unify ra rb
      (TyRecord x, TyRecord y) -> unify x y
      _ -> do
        tta <- pointToType a
        ttb <- pointToType b
        throwError $ "Types do not match: " <> (pack . show) tta <> ", " <> (pack . show) ttb

  where
    bind :: Monad m => TypeVar -> Point -> Point -> InferT m ()
    bind x a' t = do
      freeTypeVarsT <- ftv t
      equivalent a' t >>= \case
        True -> pure ()
        False -> case x `Set.member` freeTypeVarsT of
          True -> do
            tta <- pointToType a
            ttb <- pointToType b
            throwError $ "Infinite type: " <> (pack . show) tta <> ", " <> (pack . show) ttb
          False -> union a' t
