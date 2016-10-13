{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Lib.Compiler.Typechecker.Types where

import           Control.Lens         hiding (Context, op)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List            (intercalate)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text, unpack)

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
newtype TypeVar = TypeVar Text
  deriving (Eq, Ord)

-- In paper: Xi
newtype TypeConstructor = TypeConstructor Text
  deriving (Eq, Ord)

newtype ClassName = ClassName Text
  deriving (Eq, Ord)

-- In paper: tau
data SimpleType
  = TyVar TypeVar
  | TyApp TypeConstructor (Maybe SimpleType)
  | TyFun SimpleType SimpleType
  | TyRecord SimpleType
  | TyRecordCons (Ref Column) SimpleType SimpleType
  | TyRecordNil
  deriving (Ord)

-- In paper: rho, "(Foo a) => ..."
newtype ConstrainedType = Constraints [(ClassName, TypeVar)] SimpleType

-- In paper: sigma, "forall a. ..."
newtype PolymorphicType = ForAll [TypeVar] ConstrainedType

instance Eq SimpleType where
  TyVar a == TyVar b = a == b
  TyApp c1 a1 == TyApp c2 a2 = c1 == c2 && a1 == a2
  TyRecord s == TyRecord t = rowMap s == rowMap t
    where rowMap :: SimpleType -> Map (Ref Column) SimpleType
          rowMap = Map.fromList . go
          go TyRecordNil = []
          go (TyVar _) = []
          go (TyRecordCons n t' rest) = (n,t') : go rest
  TyRecordCons _ _ _ == TyRecordCons _ _ _ = error "eq SimpleType: should not happen"
  TyRecordNil == TyRecordNil = True
  _ == _ = False

instance Show Kind where
  show KindStar = "*"
  show (KindFun from to) = "(" <> show from <> " -> " <> show to <> ")"

instance Show TypeVar where
  show (TypeVar a) = unpack a

instance Show TypeConstructor where
  show (TypeConstructor name) = unpack name

instance Show ClassName where
  show (ClassName name) = unpack name

instance Show SimpleType where
  show (TyVar a) = show a
  show (TyApp cons marg) = case arg of
    Just arg -> "(" <> show cons <> " " <> show arg <> ")"
    Nothing  -> show cons
  show (TyFun a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TyRecord r) = "{" <> show r <> "}"
  show (TyRecordCons name t r) = show name <> " : " <> show t <> ", " <> show r
  show (TyRecordNil) = "-"

instance Show ConstrainedType where
  show (Constraints consts body) =
      "(" <> intercalate ", " (map showConst consts) <> " => " <> show body
    where showConst (cls, t) = show cls <> " " <> show t

instance Show PolymorphicType where
  show (ForAll as t) =
    "forall " <> intercalate " " (map show as) <> ". " <> show t

--

-- Names by which the implementations can be accessed in the interpretation env
data TypeClassDict = TypeClassDict Name

data Context = Context
  { _contextVariables        :: Map Name PolymorphicType
  , _contextTypeConstructors :: Map TypeConstructor Kind
  , _contextTypeClasses      :: Map ClassName (Map Name PolymorphicType)
  , _contextTypeClassDicts   :: Map ClassName (Map SimpleType TypeClassDict)
  }
  deriving (Show)

extend :: (Name, Scheme) -> Context -> Context
extend (x, s) (Context env) = Context $ Map.insert x s env

remove :: Name -> Context -> Context
remove x (Context env) = Context $ Map.delete x env

getScheme :: Name -> Context -> Maybe Scheme
getScheme x (Context env) = Map.lookup x env

--

type Substitution = Map TypeVar SimpleType

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s2 s1 = Map.map (apply s2) s1 `Map.union` s2

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TVar

instance Substitutable Type where
  apply s t@(TyVar a)      = Map.findWithDefault t a s
  apply _ (TyNullary s)    = TyNullary s
  apply s (TyUnary n t)    = TyUnary n (apply s t)
  apply s (TyArr t1 t2)    = TyArr (apply s t1) (apply s t2)
  apply s (TyRecord r)     = TyRecord (apply s r)
  apply s (TyRow name t r) = TyRow name (apply s t) (apply s r)
  apply _ (TyNoRow)        = TyNoRow

  ftv (TyVar a)     = Set.singleton a
  ftv (TyNullary _) = Set.empty
  ftv (TyUnary _ t) = ftv t
  ftv (TyArr t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TyRecord r)  = ftv r
  ftv (TyRow _ t r) = ftv t `Set.union` ftv r
  ftv (TyNoRow)     = Set.empty

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

data TypedExpr = TExpr ::: Type

--

data TypecheckEnv m = TypecheckEnv
  { envResolveColumnRef        :: Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Id Column, DataCol))
  , envResolveTableRef         :: Ref Table -> m (Maybe (Id Table, [(Id Column, DataCol)]))
  , envGetTableRows            :: Id Table -> m Type
  , envOwnTableId              :: Id Table
  }

--

data InferState = InferState
  { _inferCount   :: Int
  , _inferContext :: Context
  }

makeLenses ''InferState

type TypeError = Text

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
