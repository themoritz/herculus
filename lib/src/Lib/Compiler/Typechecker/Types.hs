{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts            #-}

module Lib.Compiler.Typechecker.Types where

import           Control.Lens          hiding (Context, op)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import Debug.Trace (traceShowM)

import Data.Foldable (foldlM)
import           Data.List             (intercalate)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid           ((<>))
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq
import           Data.Text             (Text, unpack, pack)
import qualified Data.UnionFind.IntMap as UF

import           Lib.Types

import           Lib.Model.Column
import           Lib.Model.Table

import           Lib.Compiler.Types

--

data Kind
  = KindStar
  | KindFun Kind Kind
  deriving (Eq, Ord)

data TypeVar = TypeVar Int Kind
  deriving (Eq, Ord)

data TypeConst = TypeConst Text Kind
  deriving (Eq, Ord)

newtype ClassName = ClassName Text
  deriving (Eq, Ord)


data Point = Point (UF.Point (MonoType Point))
data Type = Type (MonoType Type)

data MonoType a
  = TyVar TypeVar
  | TyConst TypeConst
  | TyApp a a
  | TyRecord a
  | TyRecordCons (Ref Column) a a
  | TyRecordNil
  deriving (Ord)

type Qualifier = (ClassName, Point)

-- "forall a. ..."
data PolyType = ForAll [TypeVar] [Qualifier] Point

data Context = Context (Map Name PolyType)

instance Eq a => Eq (MonoType a) where
  TyVar a == TyVar b = a == b
  TyApp c1 a1 == TyApp c2 a2 = c1 == c2 && a1 == a2
  TyRecord s == TyRecord t = False --rowMap s == rowMap t
    -- where rowMap :: a -> Map (Ref Column) a
    --       rowMap = Map.fromList . go
    --       go TyRecordNil = []
    --       go (TyVar _) = []
    --       go (TyRecordCons n t' rest) = (n,t') : go rest
  TyRecordCons _ _ _ == TyRecordCons _ _ _ = error "eq SimpleType: should not happen"
  TyRecordNil == TyRecordNil = True
  _ == _ = False

instance Show Type where
  show (Type t) = case t of
    TyVar a -> show a
    TyConst c -> show c
    TyApp (Type (TyApp (Type (TyConst (TypeConst "(->)" _))) a)) b -> "(" <> show a <> " -> " <> show b <> ")"
    TyApp f arg -> "(" <> show f <> " " <> show arg <> ")"
    TyRecord r -> "{" <> show r <> "}"
    TyRecordCons name t r -> show name <> " : " <> show t <> ", " <> show r
    TyRecordNil -> "-"

instance Show Kind where
  show KindStar = "*"
  show (KindFun arg res) = "(" <> show arg <> " -> " <> show res <> ")"

instance Show TypeVar where
  show (TypeVar a k) = show a -- <> " : " <> show k

instance Show TypeConst where
  show (TypeConst n k) = unpack n -- <> " : " <> show k

instance Show ClassName where
  show (ClassName name) = unpack name

-- instance Show PolyType where
  -- show (ForAll as qs t) =
      -- "forall " <> intercalate " " (map show as) <> ". " <>
      -- "(" <> intercalate ", " (map showQuali qs) <> ")" <>
      -- "=> " <> show t
    -- where showQuali (cls, t') = show cls <> " " <> show t'

-- Names by which the implementations can be accessed in the interpretation env
data TypeClassDict = TypeClassDict Name

data InferState = InferState
  { _inferContext          :: Context
  , _inferCount            :: Int
  , _inferPointSupply      :: UF.PointSupply (MonoType Point)

  -- Later ...
  -- , _inferTypeClasses      :: Map ClassName (Map Name PolyType)
  -- , _inferTypeClassDicts   :: Map ClassName (Map (MonoType Point) TypeClassDict)
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

instance Types PolyType where
  ftv (ForAll _ _ pt) = ftv pt

instance Types Point where
  ftv pt = findType pt >>= ftv

instance Types Type where
  ftv (Type t) = ftv t

instance Types Context where
  ftv (Context m) = foldlM (\vs poly -> Set.union <$> pure vs <*> ftv poly) Set.empty m


lookupName :: (MonadError TypeError m, MonadState InferState m) => Name -> m PolyType
lookupName name = do
  Context ctx <- use inferContext
  case Map.lookup name ctx of
    Just t -> pure t
    Nothing -> throwError $ "not in scope: " <> name

inLocalContext :: MonadState InferState m => (Name, PolyType) -> m a -> m a
inLocalContext (name, poly) m = do
  oldContext <- use inferContext
  inferContext %= \(Context ctx) -> Context $ Map.insert name poly ctx
  res <- m
  inferContext .= oldContext
  pure res

fresh :: MonadState InferState m => m Point
fresh = do
  c <- inferCount <+= 1
  mkPoint $ TyVar $ TypeVar c KindStar

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

pointToType :: MonadState InferState m => Point -> m Type
pointToType p = do
  unwrapped <- findType p
  t <- case unwrapped of
    TyVar v -> pure $ TyVar v
    TyConst c -> pure $ TyConst c
    TyApp l r -> TyApp <$> pointToType l <*> pointToType r
    TyRecord r -> TyRecord <$> pointToType r
    TyRecordCons ref t r -> TyRecordCons <$> pure ref <*> pointToType t <*> pointToType r
    TyRecordNil -> pure TyRecordNil
  pure $ Type t

union :: MonadState InferState m => Point -> Point -> m ()
union (Point a) (Point b) = inferPointSupply %= \s -> UF.union s a b

--

data TypecheckEnv m = TypecheckEnv
  { envResolveColumnRef        :: Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveTableRef         :: Ref Table -> m (Maybe (Id Table, [(Id Column, DataCol)]))
  , envGetTableRows            :: Id Table -> m (MonoType Point)
  , envOwnTableId              :: Id Table
  }

--

type TypeError = Text
data TypedExpr = TExpr ::: Point

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
