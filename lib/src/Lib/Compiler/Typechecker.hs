{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lib.Compiler.Typechecker
  ( runInfer
  , infer
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
    xPoint <- freshPoint
    e' ::: bodyPoint <- inLocalContext (x, ForAll [] [] xPoint) (infer e)
    arrPoint <- arr xPoint bodyPoint
    pure $ TLam x e' ::: arrPoint
  PApp f arg -> do
    f' ::: fPoint <- infer f
    arg' ::: argPoint <- infer arg
    resultPoint <- freshPoint
    arrPoint <- arr argPoint resultPoint
    unify fPoint arrPoint
    pure $ TApp f' arg' ::: resultPoint
  PLet x e rest -> do
    e' ::: ePoint <- infer e
    poly <- generalize ePoint
    rest' ::: restPoint <- inLocalContext (x, poly) (infer rest)
    pure $ TLet x e' rest' ::: restPoint
  PIf cond e1 e2 -> do
    cond' ::: condPoint <- infer cond
    e1' ::: e1Point <- infer e1
    e2' ::: e2Point <- infer e2
    boolPoint <- mkPoint tyBool
    unify condPoint boolPoint
    unify e1Point e2Point
    pure $ TIf cond' e1' e2' ::: e1Point
  PVar x -> do
    poly <- lookupName x
    xPoint <- instantiate poly
    pure $ TVar x ::: xPoint
  PLit l -> do
    t <- case l of
      LNumber _ -> mkPoint tyNumber
      LBool _   -> mkPoint tyBool
      LString _ -> mkPoint tyString
    pure $ TLit l ::: t
  PPrjRecord e name -> do
    e' ::: ePoint <- infer e
    resultPoint <- freshPoint
    tailPoint <- freshPoint
    consPoint <- mkPoint $ TyRecordCons name resultPoint tailPoint
    recordPoint <- mkPoint $ TyRecord consPoint
    s <- unify ePoint recordPoint
    pure $ TPrjRecord e' name ::: resultPoint
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
  pool <- mapM (\a -> (a,) <$> freshPoint) as
  let replace p' = do
        t <- findType p'
        case t of
          TyVar a -> case lookup a pool of
            Nothing -> pure p'
            Just fresh -> pure fresh
          TyConst _  -> pure p'
          TyApp l r -> (TyApp <$> replace l <*> replace r) >>= mkPoint
          TyRecord r -> (TyRecord <$> replace r) >>= mkPoint
          TyRecordCons n h t' -> (TyRecordCons n <$> replace h <*> replace t') >>= mkPoint
          TyRecordNil -> pure p'
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
      (TyRecordCons n1 h1 t1, TyRecordCons n2 h2 t2)
        | n1 == n2 -> do
            unify h1 h2
            unify t1 t2
        | otherwise -> do
            deferredTail <- freshPoint
            alternativeTail1 <- mkPoint $ TyRecordCons n1 h1 deferredTail
            alternativeTail2 <- mkPoint $ TyRecordCons n2 h2 deferredTail
            -- TODO: Explain why switched
            unify t1 alternativeTail2
            unify t2 alternativeTail1
      _ -> do
        typeA <- pointToType a
        typeB <- pointToType b
        throwError $ "Types do not match: " <> (pack . show) typeA <> ", " <> (pack . show) typeB

  where
    bind :: Monad m => TypeVar -> Point -> Point -> InferT m ()
    bind x pointOfX otherPoint = do
      otherFtv <- ftv otherPoint
      equivalent pointOfX otherPoint >>= \case
        True -> pure ()
        False -> case x `Set.member` otherFtv of
          True -> do
            typeOfX <- pointToType pointOfX
            otherType <- pointToType otherPoint
            throwError $ "Infinite type: " <> (pack . show) typeOfX <> ", " <> (pack . show) otherType
          False -> union pointOfX otherPoint
