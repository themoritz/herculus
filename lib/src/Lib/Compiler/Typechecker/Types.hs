{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}

module Lib.Compiler.Typechecker.Types where

import           Control.Lens          hiding (Context, op)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Debug.Trace (traceShowM, traceM)

import           Data.Foldable (foldlM)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid           ((<>))
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (pack, unpack)
import qualified Data.UnionFind.IntMap as UF

import           Lib.Types

import           Lib.Model.Column
import           Lib.Model.Table

import           Lib.Compiler.Types

--

data Context = Context
  { _contextTypes :: Map Name (PolyType Point)
  , _contextInstanceDicts :: Map (Predicate Type) Name -- predicate -> instance dictionary
  }

makeLenses ''Context

data InferState = InferState
  { _inferContext          :: Context
  , _inferCount            :: Int
  , _inferPointSupply      :: UF.PointSupply (MonoType Point)
  }

makeLenses ''InferState

class Types t where
  ftv :: MonadState InferState m => t -> m (Set TypeVar)

instance Types TypeVar where
  ftv v = pure $ Set.singleton v

instance Types TypeConst where
  ftv _ = pure $ Set.empty

instance Types a => Types (MonoType a) where
  ftv (TyVar v) = ftv v
  ftv (TyConst c) = ftv c
  ftv (TyApp l r) = Set.union <$> ftv l <*> ftv r
  ftv (TyRecord r) = ftv r
  ftv (TyRecordCons _ t r) = Set.union <$> ftv t <*> ftv r
  ftv (TyRecordNil) = pure Set.empty

instance Types a => Types [a] where
  ftv = foldlM (\vs a -> Set.union <$> pure vs <*> ftv a) Set.empty

instance Types a => Types (PolyType a) where
  -- TODO: Understand: need to include ftv of preds?
  ftv (ForAll as preds pt) = Set.difference <$> (Set.union <$> ftv pt <*> ftv preds) <*> ftv as

instance Types Point where
  ftv pt = findType pt >>= ftv

instance Types Type where
  ftv (Type t) = ftv t

instance Types a => Types (Predicate a) where
  ftv (IsIn _ t) = ftv t

instance Types Context where
  ftv (Context types _) = foldlM (\vs poly -> Set.union <$> pure vs <*> ftv poly) Set.empty types


lookupPolyType :: (MonadError TypeError m, MonadState InferState m) => Name -> m (PolyType Point)
lookupPolyType name = do
  ctx <- use (inferContext . contextTypes)
  case Map.lookup name ctx of
    Just t -> pure t
    Nothing -> throwError $ "not in scope: " <> name

inLocalContext :: MonadState InferState m => (Name, PolyType Point) -> m a -> m a
inLocalContext (name, poly) action = do
  oldContext <- use (inferContext . contextTypes)
  inferContext . contextTypes %= Map.insert name poly
  res <- action
  inferContext . contextTypes .= oldContext
  pure res

lookupInstanceDict :: MonadState InferState m => Predicate Point -> m (Maybe Name)
lookupInstanceDict predicate = do
  typePred <- predicateFromPoint predicate
  dicts <- use (inferContext . contextInstanceDicts)
  pure $ Map.lookup typePred dicts

-- no unification within action!
withInstanceDicts :: MonadState InferState m => [(Predicate Point, Name)] -> m a -> m a
withInstanceDicts dicts action = do
  old <- use (inferContext . contextInstanceDicts)
  typeDicts <- mapM (\(p, n) -> (,n) <$> predicateFromPoint p) dicts
  inferContext . contextInstanceDicts %= Map.union (Map.fromList typeDicts)
  res <- action
  inferContext . contextInstanceDicts .= old
  pure res

freshPoint :: MonadState InferState m => m Point
freshPoint = do
  c <- inferCount <+= 1
  mkPoint $ TyVar $ TypeVar c KindStar

freshDictName :: MonadState InferState m => ClassName -> m Name
freshDictName (ClassName n) = do
  c <- inferCount <+= 1
  pure $ "$" <> n <> (pack . show) c

mkPoint :: MonadState InferState m => MonoType Point -> m Point
mkPoint c = do
  supply <- use inferPointSupply
  let (supply', point) = UF.fresh supply c
  inferPointSupply .= supply'
  pure $ Point point

find :: MonadState InferState m => Point -> m Point
find (Point p) = (Point . flip UF.repr p) <$> use inferPointSupply

findType :: MonadState InferState m => Point -> m (MonoType Point)
findType (Point p) = (flip UF.descriptor p) <$> use inferPointSupply

equivalent :: MonadState InferState m => Point -> Point -> m Bool
equivalent (Point a) (Point b) = (\supply -> UF.equivalent supply a b) <$> use inferPointSupply

debugPoint :: MonadState InferState m => Point -> m ()
debugPoint p = do
  t <- pointToType p
  traceShowM t

debugTExpr :: MonadState InferState m => TExpr -> m ()
debugTExpr expr = debugTExpr' expr >>= mapM_ traceM
  where
    debugTExpr' = \case
      TLam x e -> do
        eLines <- debugTExpr' e
        pure $ ["\\" <> unpack x <> " ->"] <> indent eLines
      TApp f arg -> do
        fLines <- debugTExpr' f
        argLines <- debugTExpr' arg
        pure $ fLines <> argLines
      TLet x e rest -> do
        eLines <- debugTExpr' e
        restLines <- debugTExpr' rest
        pure $ ["let " <> unpack x <> " ="] <> indent eLines <> [";"] <> indent restLines
      TIf c t e -> do
        cLines <- debugTExpr' c
        tLines <- debugTExpr' t
        eLines <- debugTExpr' e
        pure $ ["if"] <> indent cLines <> ["then"] <> indent tLines <> ["else"] <> indent eLines
      TVar x -> pure [unpack x]
      TLit l -> pure [show l]
      TPrjRecord e c -> do
        eLines <- debugTExpr' e
        pure $ ["Access " <> show c <> " of:"] <> indent eLines
      TWithPredicates preds e -> do
        preds' <- mapM predicateFromPoint preds
        eLines <- debugTExpr' e
        pure $ ["With:"] <> indent (map show preds') <> ["=>"] <> indent eLines
      TTypeClassDict predicate -> do
        predicate' <- predicateFromPoint predicate
        pure ["Dict: " <> show predicate']
      TColumnRef c -> pure ["Column: " <> show c]
      TWholeColumnRef c -> pure ["Column: " <> show c]
      TTableRef t cs -> pure ["Table: " <> show t <> ", columns: " <> show cs]
    indent = map ("  " <>)

pointToType :: MonadState InferState m => Point -> m Type
pointToType p = do
  unwrapped <- findType p
  t <- case unwrapped of
    TyVar v -> pure $ TyVar v
    TyConst c -> pure $ TyConst c
    TyApp l r -> TyApp <$> pointToType l <*> pointToType r
    TyRecord r -> TyRecord <$> pointToType r
    TyRecordCons ref t r -> TyRecordCons ref <$> pointToType t <*> pointToType r
    TyRecordNil -> pure TyRecordNil
  pure $ Type t

typeToPoint :: MonadState InferState m => Type -> m Point
typeToPoint (Type t) = case t of
  TyVar v -> mkPoint $ TyVar v
  TyConst c -> mkPoint $ TyConst c
  TyApp l r -> (TyApp <$> typeToPoint l <*> typeToPoint r) >>= mkPoint
  TyRecord r -> (TyRecord <$> typeToPoint r) >>= mkPoint
  TyRecordCons ref t' r -> (TyRecordCons ref <$> typeToPoint t' <*> typeToPoint r) >>= mkPoint
  TyRecordNil -> mkPoint TyRecordNil

predicateFromPoint :: MonadState InferState m => Predicate Point -> m (Predicate Type)
predicateFromPoint (IsIn c p) = IsIn c <$> pointToType p

predicateToPoint :: MonadState InferState m => Predicate Type -> m (Predicate Point)
predicateToPoint (IsIn c t) = IsIn c <$> typeToPoint t

polyFromPoint :: MonadState InferState m => PolyType Point -> m (PolyType Type)
polyFromPoint (ForAll as preds point) = do
  let t = pointToType point
      transformPred (IsIn c p) = IsIn c <$> pointToType p
  ForAll as <$> mapM transformPred preds <*> t

polyToPoint :: MonadState InferState m => PolyType Type -> m (PolyType Point)
polyToPoint (ForAll as preds t) = do
  let point = typeToPoint t
      transformPred (IsIn c p) = IsIn c <$> typeToPoint p
  ForAll as <$> mapM transformPred preds <*> point

union :: MonadState InferState m => Point -> Point -> m ()
union (Point a) (Point b) = inferPointSupply %= \s -> UF.union s a b

--

data TypecheckEnv m = TypecheckEnv
  { envResolveColumnRef        :: Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveTableRef         :: Ref Table -> m (Maybe (Id Table, [(Id Column, DataCol)]))
  , envGetTableRows            :: Id Table -> m Type
  , envOwnTableId              :: Id Table
  }

--

data TypedExpr = TExpr ::: ([Predicate Point], Point)

newtype InferT m a = InferT
  { unInferT :: ReaderT (TypecheckEnv m) (StateT InferState (ExceptT TypeError m)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (TypecheckEnv m)
             , MonadError TypeError
             , MonadState InferState
             )

instance MonadTrans InferT where
  lift = InferT . lift . lift . lift
