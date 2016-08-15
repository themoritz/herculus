{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Lib.Compiler.Parser
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
  = TBool
  | TNumber
  | TString
  deriving (Show, Eq, Ord)

data TUnary
  = TList
  | TMaybe
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TNullary TNullary
  | TUnary TUnary Type
  | TArr Type Type
  | TRecord Type
  | TRow (Ref Column) Type Type
  | TNoRow
  deriving (Eq, Ord)

instance Show Type where
  show (TVar a) = show a
  show (TNullary c) = show c
  show (TUnary t t1) = show t <> " " <> show t1
  show (TArr a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TRecord r) = "{" <> show r <> "}"
  show (TRow name t r) = show name <> " : " <> show t <> ", " <> show r
  show (TNoRow) = "-"

data Scheme = Forall [TVar] Type

instance Show Scheme where
  show (Forall as t) = "forall " <> intercalate " " (map show as) <> ". " <> show t

typeOfDataType :: DataType -> Type
typeOfDataType DataString = TNullary TString
typeOfDataType DataNumber = TNullary TNumber
typeOfDataType DataBool = TNullary TBool

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
  apply s t@(TVar a)      = Map.findWithDefault t a s
  apply _ (TNullary s)    = TNullary s
  apply s (TUnary n t)    = TUnary n (apply s t)
  apply s (TArr t1 t2)    = TArr (apply s t1) (apply s t2)
  apply s (TRecord r)     = TRecord (apply s r)
  apply s (TRow name t r) = TRow name (apply s t) (apply s r)
  apply _ (TNoRow)        = TNoRow

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

data TypedExpr = Expr Id ::: Type

--

data TypecheckEnv m = TypecheckEnv
  { envResolveColumnRef        :: Ref Column -> m (Maybe (Entity Column))
  , envResolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Id Table, Entity Column))
  , envResolveTableRef         :: Ref Table -> m (Maybe (Id Table, Map (Ref Column) Type))
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
