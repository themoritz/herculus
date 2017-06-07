{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- |

module Lib.Compiler.Check.Monad.Types where

import           Lib.Prelude

import           Control.Lens
import           Control.Monad.Free

import           Data.Map                       (Map)
import qualified Data.Map                       as Map

import           Lib.Compiler.AST.Position
import           Lib.Compiler.Check.Error.Types
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Type
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

newtype Check a = Check
  { unCheck :: Free Check' a
  } deriving (Functor, Applicative, Monad)

type Check' = CheckF :+: ResolveF

liftCheck :: (f :<: Check') => f a -> Check a
liftCheck = Check . liftF . inj

foldCheck :: Monad m => (forall x. Check' x -> m x) -> Check a -> m a
foldCheck go = foldFree go . unCheck

--------------------------------------------------------------------------------

data CheckF a
  = FreshKind (Kind -> a)
  | FreshType (Type -> a)
  | FreshName (Text -> a)
  --
  | UnifyKinds Span Kind Kind a
  | UnifyTypes Type Type (Either CheckError () -> a)
  --
  | LookupKind Text (Maybe Kind -> a)
  | LookupType Text (Maybe (EnvType, Text) -> a)
  | LookupInstanceDict DictLookup (Maybe Text -> a)
  | LookupClass Text (Maybe Class -> a)
  --
  | GetKindSubst (KindSubst -> a)
  | GetTypeSubst (TypeSubst -> a)
  | GetCheckEnv (CheckEnv -> a)
  --
  | AddClass Text Class a
  | AddInstance Text Instance a
  | AddOperatorAlias Text Text Fixity a
  | forall b. InExtendedEnv CheckEnv (Check b) (b -> a)
  | forall b. InExtendedKindEnv (Map Text Kind) (Check b) (b -> a)
  | forall b. InExtendedTypeEnv (Map Text EnvType) (Check b) (b -> a)
  | forall b. InExtendedInstanceEnv (Map DictLookup Text) (Check b) (b -> a)
  | forall b. Retain (Check b) (b -> a)
  --
  | DebugEnv a
  | DebugTypeSubst a
  --
  | forall b. ThrowError Error (b -> a)
  | forall b. CatchError (Check b) (Error -> Check b) (b -> a)

deriving instance Functor CheckF

instance MonadError Error Check where
  throwError e = liftCheck $ ThrowError e id
  catchError m h = liftCheck $ CatchError m h id

freshKind :: Check Kind
freshKind = liftCheck $ FreshKind id

freshType :: Check Type
freshType = liftCheck $ FreshType id

freshName :: Check Text
freshName = liftCheck $ FreshName id

unifyKinds :: Span -> Kind -> Kind -> Check ()
unifyKinds span k1 k2 = liftCheck $ UnifyKinds span k1 k2 ()

unifyTypes :: Type -> Type -> Check (Either CheckError ())
unifyTypes t1 t2 = liftCheck $ UnifyTypes t1 t2 id

lookupKind :: Text -> Check (Maybe Kind)
lookupKind t = liftCheck $ LookupKind t id

lookupType :: Text -> Check (Maybe (EnvType, Text))
lookupType t = liftCheck $ LookupType t id

lookupInstanceDict :: DictLookup -> Check (Maybe Text)
lookupInstanceDict c = liftCheck $ LookupInstanceDict c id

lookupClass :: Text -> Check (Maybe Class)
lookupClass n = liftCheck $ LookupClass n id

addClass :: Text -> Class -> Check ()
addClass n c = liftCheck $ AddClass n c ()

addInstance :: Text -> Instance -> Check ()
addInstance c i = liftCheck $ AddInstance c i ()

addOperatorAlias :: Text -> Text -> Fixity -> Check ()
addOperatorAlias o a f = liftCheck $ AddOperatorAlias o a f ()

getKindSubst :: Check KindSubst
getKindSubst = liftCheck $ GetKindSubst id

getTypeSubst :: Check TypeSubst
getTypeSubst = liftCheck $ GetTypeSubst id

getCheckEnv :: Check CheckEnv
getCheckEnv = liftCheck $ GetCheckEnv id

inExtendedEnv :: CheckEnv -> Check a -> Check a
inExtendedEnv env m = liftCheck $ InExtendedEnv env m id

inExtendedKindEnv :: Map Text Kind -> Check a -> Check a
inExtendedKindEnv env m = liftCheck $ InExtendedKindEnv env m id

inExtendedTypeEnv :: Map Text EnvType -> Check a -> Check a
inExtendedTypeEnv env m = liftCheck $ InExtendedTypeEnv env m id

inExtendedInstanceEnv :: Map DictLookup Text -> Check a -> Check a
inExtendedInstanceEnv env m = liftCheck $ InExtendedInstanceEnv env m id

retain :: Check a -> Check a
retain m = liftCheck $ Retain m id

debugEnv :: Check ()
debugEnv = liftCheck $ DebugEnv ()

debugTypeSubst :: Check ()
debugTypeSubst = liftCheck $ DebugTypeSubst ()

--------------------------------------------------------------------------------

type Resolver m = forall x. ResolveF x -> m x

data ResolveF a
  = GetTableRecordType (Id Table) (Map Text Type -> a)
  | GetTableName (Id Table) (Text -> a)
  | ResolveColumnOfTableRef (Ref Table) (Ref Column)
                            (Maybe (Id Table, Id Column, DataCol) -> a)
  | ResolveColumnRef (Ref Column) (Maybe (Id Column, DataCol) -> a)
  | ResolveTableRef (Ref Table) (Maybe (Id Table) -> a)
  deriving (Functor)

getTableRecordType :: Id Table -> Check (Map Text Type)
getTableRecordType t = liftCheck $ GetTableRecordType t id

getTableName :: Id Table -> Check Text
getTableName t = liftCheck $ GetTableName t id

resolveColumnOfTableRef
  :: Ref Table -> Ref Column -> Check (Maybe (Id Table, Id Column, DataCol))
resolveColumnOfTableRef t c = liftCheck $ ResolveColumnOfTableRef t c id

resolveColumnRef :: Ref Column -> Check (Maybe (Id Column, DataCol))
resolveColumnRef c = liftCheck $ ResolveColumnRef c id

resolveTableRef :: Ref Table -> Check (Maybe (Id Table))
resolveTableRef t = liftCheck $ ResolveTableRef t id


--------------------------------------------------------------------------------

data Origin
  = Recursive
  | Default
  | Method
  deriving (Show)

data EnvType = EnvType
  { etPoly   :: PolyType
  , etOrigin :: Origin
  }

defaultEnvType :: PolyType -> EnvType
defaultEnvType = flip EnvType Default

data DictLookup
  -- | Class name, type variable
  = ByTypeVar Text Text
  -- | Class name, constructor name
  | ByConstructor Text Text
  -- | Type variable
  | AccessByTypeVar Text
  deriving (Eq, Ord, Show)

data CheckEnv = CheckEnv
  { _checkEnvKinds         :: Map Text Kind
  , _checkEnvTypes         :: Map Text EnvType
  , _checkEnvOperators     :: Map Text (Text, Fixity)
  , _checkEnvInstanceDicts :: Map DictLookup Text
  , _checkEnvClasses       :: Map Text Class
  }

mkCheckEnvOpTable :: CheckEnv -> OpTable
mkCheckEnvOpTable = Map.toList . map snd . _checkEnvOperators

unionCheckEnv :: CheckEnv -> CheckEnv -> CheckEnv
unionCheckEnv (CheckEnv k t o i c) (CheckEnv k' t' o' i' c') =
  CheckEnv (Map.union k k')
           (Map.union t t')
           (Map.union o o')
           (Map.union i i')
           (Map.union c c')

primCheckEnv :: CheckEnv
primCheckEnv = CheckEnv primKindEnv primTypeEnv' Map.empty Map.empty Map.empty
  where primTypeEnv' = map defaultEnvType primTypeEnv

makeLenses ''CheckEnv

data CheckState = CheckState
  { _checkEnv       :: CheckEnv
  , _checkCount     :: Int
  , _checkTypeSubst :: TypeSubst
  , _checkKindSubst :: KindSubst
  }

makeLenses ''CheckState

type CheckInterpT m = StateT CheckState (ExceptT Error m)
