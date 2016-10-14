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

-- In paper: k
data Kind
  = KindStar
  | KindFun Kind Kind
  deriving (Eq, Ord)

-- In paper: alpha
newtype TypeVar = TypeVar Int
  deriving (Eq, Ord)

-- In paper: Xi
newtype TypeConst = TypeConst Text
  deriving (Eq, Ord)

newtype ClassName = ClassName Text
  deriving (Eq, Ord)

-- In paper: tau
data MonoType
  = TyVar TypeVar
  | TyConst TypeConst
  | TyApp MonoType MonoType
  | TyFun MonoType MonoType
  | TyRecord MonoType
  | TyRecordCons (Ref Column) MonoType MonoType
  | TyRecordNil
  deriving (Ord)

type Qualifier = (ClassName, MonoType)

-- In paper: sigma, "forall a. ..."
data PolyType = ForAll [TypeVar] [Qualifier] MonoType

instance Eq MonoType where
  TyVar a == TyVar b = a == b
  TyApp c1 a1 == TyApp c2 a2 = c1 == c2 && a1 == a2
  TyRecord s == TyRecord t = rowMap s == rowMap t
    where rowMap :: MonoType -> Map (Ref Column) MonoType
          rowMap = Map.fromList . go
          go TyRecordNil = []
          go (TyVar _) = []
          go (TyRecordCons n t' rest) = (n,t') : go rest
  TyRecordCons _ _ _ == TyRecordCons _ _ _ = error "eq SimpleType: should not happen"
  TyRecordNil == TyRecordNil = True
  _ == _ = False

instance Show Kind where
  show KindStar = "*"
  show (KindFun arg res) = "(" <> show arg <> " -> " <> show res <> ")"

instance Show TypeVar where
  show (TypeVar a) = show a

instance Show TypeConst where
  show (TypeConst name) = unpack name

instance Show ClassName where
  show (ClassName name) = unpack name

instance Show MonoType where
  show (TyVar a) = show a
  show (TyConst c) = show c
  show (TyApp f arg) = "(" <> show f <> " " <> show arg <> ")"
  show (TyFun a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TyRecord r) = "{" <> show r <> "}"
  show (TyRecordCons name t r) = show name <> " : " <> show t <> ", " <> show r
  show (TyRecordNil) = "-"

instance Show PolyType where
  show (ForAll as qs t) =
      "forall " <> intercalate " " (map show as) <> ". " <>
      "(" <> intercalate ", " (map showQuali qs) <> ")" <>
      "=> " <> show t
    where showQuali (cls, t') = show cls <> " " <> show t'

--

data Constraint
  = Unify Point Point
  | Generalize Name Point (Set TypeVar)
  | Instantiate Point (Either Name PolyType)
  | Skolemize Point (Either Name PolyType) (Set TypeVar)
  | ProveQualifier Qualifier
  | AssumeQualifier Qualifier

type Point = UF.Point MonoType

-- Names by which the implementations can be accessed in the interpretation env
data TypeClassDict = TypeClassDict Name

data InferState = InferState
  { _inferContext          :: Map Name PolyType
  , _inferCount            :: Int
  , _inferPointSupply      :: UF.PointSupply MonoType
  , _inferConstraints      :: Seq Constraint

  -- Later ...
  , _inferTypeConstructors :: Map TypeConst Kind
  -- ^ Kinds of type constructors, eg List : * -> *
  , _inferTypeClasses      :: Map ClassName (Map Name PolyType)
  , _inferTypeClassDicts   :: Map ClassName (Map MonoType TypeClassDict)
  }

makeLenses ''InferState

lookupPolyType :: (MonadError TypeError m, MonadState InferState m) => Name -> m PolyType
lookupPolyType name = do
  ctx <- use inferContext
  case Map.lookup name ctx of
    Just t -> pure t
    Nothing -> throwError $ "not in scope: " <> name

inLocalContext :: MonadState InferState m => (Name, PolyType) -> m a -> m a
inLocalContext (name, poly) m = do
  oldContext <- use inferContext
  inferContext %= Map.insert name poly
  res <- m
  inferContext .= oldContext
  pure res

fresh :: MonadState InferState m => m Point
fresh = do
  c <- inferCount <+= 1
  supply <- use inferPointSupply
  let (supply', point) = UF.fresh supply $ TyVar $ TypeVar c
  inferPointSupply .= supply'
  pure point

find :: MonadState InferState m => Point -> m Point
find p = (flip UF.repr p) <$> use inferPointSupply

findType :: MonadState InferState m => Point -> m MonoType
findType p = (flip UF.descriptor p) <$> use inferPointSupply

union :: MonadState InferState m => Point -> Point -> m ()
union a b = inferPointSupply %= \s -> UF.union s a b

logConstraint :: MonadState InferState m => Constraint -> m ()
logConstraint c = inferConstraints %= flip (Seq.|>) c

--

data TypecheckEnv m = TypecheckEnv
  { envResolveColumnRef        :: Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveTableRef         :: Ref Table -> m (Maybe (Id Table, [(Id Column, DataCol)]))
  , envGetTableRows            :: Id Table -> m MonoType
  , envOwnTableId              :: Id Table
  }

--

type TypeError = Text
data TypedExpr = TExpr ::: MonoType

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
