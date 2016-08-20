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

import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Types

--

newtype TVar = TV Text
  deriving (Eq, Ord)

instance Show TVar where
  show (TV a) = unpack a

data TNullary
  = TyBool
  | TyNumber
  | TyString
  deriving (Show, Eq, Ord)

data TUnary
  = TyList
  | TyMaybe
  deriving (Show, Eq, Ord)

data Type
  = TyVar TVar
  | TyNullary TNullary
  | TyUnary TUnary Type
  | TyArr Type Type
  | TyRecord Type
  | TyRow (Ref Column) Type Type
  | TyNoRow
  deriving (Ord)

instance Eq Type where
  TyVar a == TyVar b = a == b
  TyNullary a == TyNullary b = a == b
  TyUnary a s == TyUnary b t = a == b && s == t
  TyArr s t == TyArr s' t' = s == s' && t == t'
  TyRecord s == TyRecord t = rowMap s == rowMap t
    where rowMap :: Type -> Map (Ref Column) Type
          rowMap = Map.fromList . go
          go TyNoRow = []
          go (TyVar _) = []
          go (TyRow n t' rest) = (n,t') : go rest
  TyNoRow == TyNoRow = True
  _ == _ = False
  

instance Show Type where
  show (TyVar a) = show a
  show (TyNullary c) = show c
  show (TyUnary t t1) = show t <> " " <> show t1
  show (TyArr a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TyRecord r) = "{" <> show r <> "}"
  show (TyRow name t r) = show name <> " : " <> show t <> ", " <> show r
  show (TyNoRow) = "-"

data Scheme = Forall [TVar] Type

instance Show Scheme where
  show (Forall as t) = "forall " <> intercalate " " (map show as) <> ". " <> show t

typeOfDataType :: Applicative m => (Id Table -> m Type) -> DataType -> m Type
typeOfDataType f = \case
  DataBool       -> pure $ TyNullary TyBool
  DataString     -> pure $ TyNullary TyString
  DataNumber     -> pure $ TyNullary TyNumber
  (DataRecord t) -> TyRecord <$> f t
  (DataList t)   -> TyUnary TyList <$> typeOfDataType f t
  (DataMaybe t)  -> TyUnary TyMaybe <$> typeOfDataType f t

--

newtype Context = Context (Map Name Scheme)
  deriving (Show)

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
  { envResolveColumnRef        :: Ref Column -> m (Maybe (Entity Column))
  , envResolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Entity Column))
  , envResolveTableRef         :: Ref Table -> m (Maybe (Id Table, [Entity Column]))
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
