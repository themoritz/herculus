{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- |

module Lib.Compiler.Check.Monad where

import           Lib.Prelude

import           Control.Lens              hiding ((:<))
import           Control.Monad.Free

import           Data.Functor.Foldable
import qualified Data.Map                  as Map

import           Lib.Compiler.AST.Position
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
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
  | UnifyTypes Span Type Type (Either Error () -> a)
  --
  | LookupKind Text (Maybe Kind -> a)
  | LookupType Text (Maybe EnvType -> a)
  | LookupInstanceDict DictLookup (Maybe Text -> a)
  | LookupClass Text (Maybe Class -> a)
  --
  | GetKindSubst (KindSubst -> a)
  | GetTypeSubst (TypeSubst -> a)
  | GetCheckEnv (CheckEnv -> a)
  --
  | AddClass Text Class a
  | AddInstance Text Instance a
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

unifyTypes :: Span -> Type -> Type -> Check (Either Error ())
unifyTypes span t1 t2 = liftCheck $ UnifyTypes span t1 t2 id

lookupKind :: Text -> Check (Maybe Kind)
lookupKind t = liftCheck $ LookupKind t id

lookupType :: Text -> Check (Maybe EnvType)
lookupType t = liftCheck $ LookupType t id

lookupInstanceDict :: DictLookup -> Check (Maybe Text)
lookupInstanceDict c = liftCheck $ LookupInstanceDict c id

lookupClass :: Text -> Check (Maybe Class)
lookupClass n = liftCheck $ LookupClass n id

addClass :: Text -> Class -> Check ()
addClass n c = liftCheck $ AddClass n c ()

addInstance :: Text -> Instance -> Check ()
addInstance c i = liftCheck $ AddInstance c i ()

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
  = GetTableRecordType (Id Table) (Type -> a)
  | ResolveColumnOfTableRef (Ref Table) (Ref Column)
                            (Maybe (Id Table, Id Column, DataCol) -> a)
  | ResolveColumnRef (Ref Column) (Maybe (Id Column, DataCol) -> a)
  | ResolveTableRef (Ref Table) (Maybe (Id Table) -> a)
  deriving (Functor)

getTableRecordType :: Id Table -> Check Type
getTableRecordType t = liftCheck $ GetTableRecordType t id

resolveColumnOfTableRef
  :: Ref Table -> Ref Column -> Check (Maybe (Id Table, Id Column, DataCol))
resolveColumnOfTableRef t c = liftCheck $ ResolveColumnOfTableRef t c id

resolveColumnRef :: Ref Column -> Check (Maybe (Id Column, DataCol))
resolveColumnRef c = liftCheck $ ResolveColumnRef c id

resolveTableRef :: Ref Table -> Check (Maybe (Id Table))
resolveTableRef c = liftCheck $ ResolveTableRef c id

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
  deriving (Eq, Ord, Show)

data CheckEnv = CheckEnv
  { _checkEnvTypes         :: Map Text EnvType
  , _checkEnvKinds         :: Map Text Kind
  , _checkEnvInstanceDicts :: Map DictLookup Text
  , _checkEnvClasses       :: Map Text Class
  }

unionCheckEnv :: CheckEnv -> CheckEnv -> CheckEnv
unionCheckEnv (CheckEnv t k i c) (CheckEnv t' k' i' c') =
  CheckEnv (Map.union t t')
           (Map.union k k')
           (Map.union i i')
           (Map.union c c')

primCheckEnv :: CheckEnv
primCheckEnv = CheckEnv primTypeEnv' primKindEnv Map.empty Map.empty
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

runCheck
  :: forall m a. Monad m
  => CheckEnv -> Resolver m
  -> Check a -> m (Either Error a)
runCheck env goResolve =
  runExceptT .
  flip evalStateT (CheckState env 0 Map.empty Map.empty) .
  foldCheck go
  where
  go :: Check' b -> CheckInterpT m b
  go = coproduct goCheck (lift . lift . goResolve)
  goCheck = \case
    FreshKind reply -> do
      i <- checkCount <+= 1
      pure $ reply $ kindUnknown i

    FreshType reply -> do
      i <- checkCount <+= 1
      pure $ reply $ typeVar (show i)

    FreshName reply -> do
      i <- checkCount <+= 1
      pure $ reply $ show i

    UnifyKinds span k1 k2 next -> unify k1 k2 $> next
      where

      unify :: Kind -> Kind -> CheckInterpT m ()
      unify k1' k2' = do
        subst <- use checkKindSubst
        let
          a'@(Fix a) = applyKindSubst subst k1'
          b'@(Fix b) = applyKindSubst subst k2'
        case (a, b) of
          (KindUnknown x, KindUnknown y) | x == y -> pure ()
          (KindUnknown x, _) -> bind x b'
          (_, KindUnknown x) -> bind x a'
          (KindType, KindType) -> pure ()
          (KindFun f arg, KindFun f' arg') -> do
            unify f f'
            unify arg arg'
          (KindRecord x, KindRecord y) -> unify x y
          _ -> compileError span $
            "Cannot match kind `" <>
            prettyKind a' <> "` with `" <>
            prettyKind b' <> "`."

      bind :: Int -> Kind -> CheckInterpT m ()
      bind i k = do
        -- Occurs check
        flip cata k $ \case
          KindUnknown v | v == i -> do
            subst <- use checkKindSubst
            let
              k1' = applyKindSubst subst k1
              k2' = applyKindSubst subst k2
            compileError span $
              "Infinite kind while trying to unify `" <>
              prettyKind k1' <> "` with `" <>
              prettyKind k2' <> "`."
          _ -> pure ()
        checkKindSubst %= kindSubstAfter (Map.singleton i k)

    UnifyTypes span t1 t2 reply -> do
      result <- runExceptT $ unify t1 t2
      pure $ reply result
      where

      unify :: Type -> Type -> ExceptT Error (CheckInterpT m) ()
      unify t1' t2' = do
        subst <- use checkTypeSubst
        let
          a'@(Fix a) = applyTypeSubst subst t1'
          b'@(Fix b) = applyTypeSubst subst t2'
        case (a, b) of
          (TypeVar x, TypeVar y) | x == y -> pure ()
          (TypeVar x, _)         -> bind x b'
          (_, TypeVar y)         -> bind y a'
          (TypeConstructor x, TypeConstructor y) | x == y -> pure ()
          (TypeRow x, TypeRow y) | x == y -> pure ()
          (TypeApp f arg, TypeApp f' arg') -> do
            unify f f'
            unify arg arg'
          (RecordCons f t rest, RecordCons f' t' rest')
            | f == f' -> do
                unify t t'
                unify rest rest'
            | otherwise -> do
                deferredRest <- lift $ foldCheck go freshType
                unify rest (recordCons f' t' deferredRest)
                unify rest' (recordCons f t deferredRest)
          (RecordNil, RecordNil) -> pure ()
          _ -> compileError span $
            "Cannot match type `" <>
            prettyType a' <> "` with `" <>
            prettyType b' <> "`."

      bind :: Text -> Type -> ExceptT Error (CheckInterpT m) ()
      bind x t = do
        -- Occurs check
        flip cata t $ \case
          TypeVar y | x == y -> do
            subst <- use checkTypeSubst
            let
              t1' = applyTypeSubst subst t1
              t2' = applyTypeSubst subst t2
            compileError span $
              "Infinite type while trying to unify `" <>
              prettyType t1' <> "` with `" <>
              prettyType t2' <> "`."
          _ -> pure ()
        checkTypeSubst %= typeSubstAfter (Map.singleton x t)

    LookupKind v reply -> do
      res <- use (checkEnv . checkEnvKinds . at v)
      pure $ reply res

    LookupType t reply -> do
      res <- use (checkEnv . checkEnvTypes . at t)
      pure $ reply res

    LookupInstanceDict c reply -> do
      res <- use (checkEnv . checkEnvInstanceDicts . at c)
      pure $ reply res

    LookupClass c reply ->
      use (checkEnv . checkEnvClasses . at c) >>= pure . reply

    AddClass n c next -> do
      checkEnv . checkEnvClasses . at n .= Just c
      pure next

    AddInstance c i next -> do
      checkEnv . checkEnvClasses . at c . _Just . _5 %= (i :)
      pure next

    GetCheckEnv reply -> use checkEnv >>= pure . reply

    GetKindSubst reply -> do
      s <- use checkKindSubst
      pure $ reply s

    GetTypeSubst reply -> do
      s <- use checkTypeSubst
      pure $ reply s

    InExtendedEnv localEnv m reply -> do
      oldEnv <- use checkEnv
      checkEnv .= unionCheckEnv localEnv oldEnv
      b <- foldCheck go m
      checkEnv .= oldEnv
      pure $ reply b

    InExtendedKindEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvKinds)
      checkEnv . checkEnvKinds .= localEnv `Map.union` oldEnv
      b <- foldCheck go m
      checkEnv . checkEnvKinds .= oldEnv
      pure $ reply b

    InExtendedTypeEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvTypes)
      checkEnv . checkEnvTypes .= localEnv `Map.union` oldEnv
      b <- foldCheck go m
      checkEnv . checkEnvTypes .= oldEnv
      pure $ reply b

    InExtendedInstanceEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvInstanceDicts)
      checkEnv . checkEnvInstanceDicts .= localEnv `Map.union` oldEnv
      b <- foldCheck go m
      checkEnv . checkEnvInstanceDicts .= oldEnv
      pure $ reply b

    Retain m reply -> do
      old <- get
      b <- foldCheck go m
      put old
      pure $ reply b

    DebugEnv next -> do
      typeEnv <- use (checkEnv . checkEnvTypes)
      subst <- use checkTypeSubst
      traceM "- Type env -"
      for_ (Map.toList typeEnv) $
        \(name, (EnvType (applyTypeSubst subst -> poly) origin)) ->
          traceM $ name <> ": " <> show origin <> " | " <> prettyPolyType poly
      traceM "------------"
      pure next

    DebugTypeSubst next -> do
      subst <- use checkTypeSubst
      traceM "- Type subst -"
      flip Map.traverseWithKey subst $ \k t ->
        traceM $ k <> " :-> " <> prettyType t
      traceM "--------------"
      pure next

    ThrowError e _ -> do
      throwError e

    CatchError m h reply -> do
      b <- foldCheck go m `catchError` (foldCheck go . h)
      pure $ reply b
