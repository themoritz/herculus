{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Lib.Compiler.Typechecker
  ( runInfer
  , infer
  , replaceTypeClassDicts
  , unify
  , newInferState
  , Type (..)
  ) where

import           Control.Lens                   hiding (Context, op)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import Debug.Trace (traceShowM)

import qualified Data.Map                       as Map
import           Data.Monoid
import qualified Data.Set                       as Set
import           Data.Text                      (pack)
import qualified Data.UnionFind.IntMap          as UF

import           Lib.Compiler.Typechecker.Prim
import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Types
import           Lib.Model.Column

--

newInferState :: InferState
newInferState = InferState
  { _inferContext = Context Map.empty Map.empty
  , _inferCount = 0
  , _inferPointSupply = UF.newPointSupply
  }

runInfer :: Monad m => TypecheckEnv m -> PExpr -> m (Either TypeError (CExpr, Type))
runInfer env expr =
  let action = do
        loadPrelude
        e ::: (_, poly) <- infer expr
        t <- pointToType poly
        e' <- replaceTypeClassDicts e
        debugTExpr e'
        c <- toCoreExpr e'
        pure (c, t)
  in  runExceptT $ evalStateT (runReaderT (unInferT action) env) newInferState

--

replaceTypeClassDicts :: Monad m => TExpr -> InferT m TExpr
replaceTypeClassDicts expr = case expr of
  TLam x e -> TLam x <$> replaceTypeClassDicts e
  TApp f e -> TApp <$> replaceTypeClassDicts f <*> replaceTypeClassDicts e
  TLet x e body -> TLet x <$> replaceTypeClassDicts e <*> replaceTypeClassDicts body
  TIf c t e -> TIf <$> replaceTypeClassDicts c <*> replaceTypeClassDicts t <*> replaceTypeClassDicts e
  TVar n -> pure $ TVar n
  TLit l -> pure $ TLit l
  TPrjRecord e r -> do
    e' <- replaceTypeClassDicts e
    pure $ TPrjRecord e' r
  TWithPredicates preds e -> do
    dicts <- mapM (\predicate@(IsIn c _) -> (predicate,) <$> freshDictName c) preds
    e' <- withInstanceDicts dicts $ replaceTypeClassDicts e
    pure $ foldr TLam e' (map snd dicts)
  TTypeClassDict predicate -> lookupInstanceDict predicate >>= \case
    Just n -> pure $ TVar n
    Nothing -> do
      IsIn cls typ <- predicateFromPoint predicate
      throwError $ "Type `" <> (pack . show) typ <> "` does not implement the `"
                            <> (pack . show) cls <> "` interface."
  TColumnRef r -> pure $ TColumnRef r
  TWholeColumnRef r -> pure $ TWholeColumnRef r
  TTableRef t c -> pure $ TTableRef t c

inferAndGeneralize :: Monad m => PExpr -> InferT m (TExpr, [Predicate Point], PolyType Point)
inferAndGeneralize e = do
  e' ::: (ePreds, ePoint) <- infer e
  (deferred, poly@(ForAll _ retained _)) <- generalize ePreds ePoint
  let e'' = TWithPredicates retained e'
  pure (e'', deferred, poly)

infer :: Monad m => PExpr -> InferT m TypedExpr
infer expr = case expr of
  PLam x e -> do
    xPoint <- freshPoint
    e' ::: (bodyPreds, bodyPoint) <- inLocalContext (x, ForAll [] [] xPoint) (infer e)
    arrPoint <- arr xPoint bodyPoint
    pure $ TLam x e' ::: (bodyPreds, arrPoint)
  PApp f arg -> do
    f' ::: (fPreds, fPoint) <- infer f
    arg' ::: (argPreds, argPoint) <- infer arg
    resultPoint <- freshPoint
    arrPoint <- arr argPoint resultPoint
    unify fPoint arrPoint
    pure $ TApp f' arg' ::: (fPreds <> argPreds, resultPoint)
  PLet x e rest -> do
    (e', deferred, poly) <- inferAndGeneralize e
    rest' ::: (restPreds, restPoint) <- inLocalContext (x, poly) $ infer rest
    pure $ TLet x e' rest' ::: (restPreds <> deferred, restPoint)
  PIf cond e1 e2 -> do
    cond' ::: (condPreds, condPoint) <- infer cond
    e1' ::: (e1Preds, e1Point) <- infer e1
    e2' ::: (e2Preds, e2Point) <- infer e2
    boolPoint <- mkPoint tyBool
    unify condPoint boolPoint
    unify e1Point e2Point
    pure $ TIf cond' e1' e2' ::: (condPreds <> e1Preds <> e2Preds, e1Point)
  PVar x -> do
    poly <- lookupPolyType x
    (xPreds, xPoint) <- instantiate poly
    -- For every predicate, insert instance dictionary node
    -- (which includes the predicate for later lookup)
    let varWithDicts = foldl TApp (TVar x) (map TTypeClassDict xPreds)
    pure $ varWithDicts ::: (xPreds, xPoint)
  PLit l -> do
    t <- case l of
      LNumber _ -> mkPoint tyNumber
      LBool _   -> mkPoint tyBool
      LString _ -> mkPoint tyString
    pure $ TLit l ::: ([], t)
  PPrjRecord e name -> do
    e' ::: (ePreds, ePoint) <- infer e
    resultPoint <- freshPoint
    tailPoint <- freshPoint
    consPoint <- mkPoint $ TyRecordCons name resultPoint tailPoint
    recordPoint <- mkPoint $ TyRecord consPoint
    unify ePoint recordPoint
    pure $ TPrjRecord e' name ::: (ePreds, resultPoint)
  PColumnRef colRef -> do
    f <- asks envResolveColumnRef
    lift (f colRef) >>= \case
      Nothing -> throwError $ pack $ "Column not found: " <> show colRef
      Just (i, dataCol) -> do
        getRows <- asks envGetTableRows
        refPoint <- (lift $ typeOfDataType getRows $ _dataColType dataCol) >>= typeToPoint
        pure $ TColumnRef i ::: ([], refPoint)
  PColumnOfTableRef tblRef colRef -> do
    f <- asks envResolveColumnOfTableRef
    lift (f tblRef colRef) >>= \case
      Nothing -> throwError $ pack $ "Column not found: " <>
                                     show colRef <> " on table " <>
                                     show tblRef
      Just (colId, dataCol) -> do
        getRows <- asks envGetTableRows
        refPoint <- (lift $ typeOfDataType getRows $ _dataColType dataCol) >>= typeToPoint
        listPoint <- mkList refPoint
        pure $ TWholeColumnRef colId ::: ([], listPoint)
  PTableRef tblRef -> do
    f <- asks envResolveTableRef
    lift (f tblRef) >>= \case
      Nothing -> throwError $ pack $ "Table not found: " <> show tblRef
      Just (i, cols) -> do
        getRows <- asks envGetTableRows
        tblRows <- (lift $ getRows i) >>= typeToPoint . Type . TyRecord
        listPoint <- mkList tblRows
        pure $ TTableRef i (map fst cols) ::: ([], listPoint)

-- Entailment

entailByInstance :: MonadState InferState m => Predicate Point -> m Bool
entailByInstance predicate = lookupInstanceDict predicate >>= \case
  Nothing -> pure False
  Just _ -> pure True

entail :: MonadState InferState m => [Predicate Point] -> Predicate Point -> m Bool
entail ps p = (||) <$> entailByInstance p <*> elemPred ps
  where
    elemPred []    = pure False
    elemPred (h:t) = (||) <$> elemPred t <*> predsEquivalent h p
    predsEquivalent (IsIn c1 p1) (IsIn c2 p2) = (&&) (c1 == c2) <$> equivalent p1 p2

-- Context reduction

reduce :: MonadState InferState m => [Predicate Point] -> m [Predicate Point]
reduce = go []
  where
    go rs [] = pure rs
    go rs (p:ps) = entail (rs <> ps) p >>= \case
      True -> go rs ps
      False -> go (p:rs) ps

--

-- | Returns a list of deferred predicates and the generalized polytype
generalize :: MonadState InferState m => [Predicate Point] -> Point -> m ([Predicate Point], PolyType Point)
generalize preds pt = do
    traceShowM ("Generalize:" :: String)
    forM_ preds $ \(IsIn c p) -> do
      traceShowM c
      debugPoint p
    debugPoint pt
    context <- use inferContext
    reducedPreds <- reduce preds
    contextVars <- ftv context
    (deferred, retained) <- partitionM reducedPreds $ \p -> do
      free <- ftv p
      pure $ all (`Set.member` contextVars) free
    let allTypeVariables = Set.union <$> ftv retained <*> ftv pt
    freeTypeVariables <- Set.difference <$> allTypeVariables <*> pure contextVars
    pure (deferred, ForAll (Set.toList freeTypeVariables) retained pt)
  where
    partitionM :: Monad m => [a] -> (a -> m Bool) -> m ([a], [a])
    partitionM [] _ = pure ([], [])
    partitionM (h:t) p = do
      (as, bs) <- partitionM t p
      p h >>= \case
        True  -> pure (h:as, bs)
        False -> pure (as, h:bs)


instantiate :: MonadState InferState m => PolyType Point -> m ([Predicate Point], Point)
instantiate (ForAll as predicates p) = do
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
      replacePred (IsIn c predicate) = IsIn c <$> replace predicate
  (,) <$> mapM replacePred predicates <*> replace p

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
            -- TODO: Understand and explain why switched
            unify t1 alternativeTail2
            unify t2 alternativeTail1
      _ -> do
        typeA <- pointToType a
        typeB <- pointToType b
        throwError $ "Types do not match: " <> (pack . show) typeA <> ", " <> (pack . show) typeB

  where
    -- TODO: check kinds are the same
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
