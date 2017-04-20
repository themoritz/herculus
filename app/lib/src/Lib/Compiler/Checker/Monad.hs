{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ViewPatterns              #-}
-- |

module Lib.Compiler.Checker.Monad where

import           Lib.Prelude

import           Control.Lens              hiding ((:<))
import           Control.Monad.Trans.Free

import           Data.Functor.Foldable
import qualified Data.Map                  as Map

import           Lib.Compiler.AST.Position
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

data CheckF a
  = FreshKind (Kind -> a)
  | FreshType (Type -> a)
  | FreshName (Text -> a)
  | UnifyKinds Span Kind Kind a
  | UnifyTypes Span Type Type (Either Error () -> a)
  | LookupKind Text (Maybe Kind -> a)
  | LookupType Text (Maybe EnvType -> a)
  | LookupInstanceDict DictLookup (Maybe Text -> a)
  | LookupClass Text (Maybe Class -> a)
  | AddClass Text Class a
  | AddInstance Text Instance a
  | GetKindSubst (KindSubst -> a)
  | GetTypeSubst (TypeSubst -> a)
  | GetCheckEnv (CheckEnv -> a)
  | forall b. InExtendedEnv CheckEnv (Check b) (b -> a)
  | forall b. InExtendedKindEnv (Map Text Kind) (Check b) (b -> a)
  | forall b. InExtendedTypeEnv (Map Text EnvType) (Check b) (b -> a)
  | forall b. InExtendedInstanceEnv (Map DictLookup Text) (Check b) (b -> a)
  | forall b. Retain (Check b) (b -> a)
  | DebugEnv a
  | DebugTypeSubst a

deriving instance Functor CheckF

type Check = FreeT CheckF (Except Error)

freshKind :: Check Kind
freshKind = liftF $ FreshKind id

freshType :: Check Type
freshType = liftF $ FreshType id

freshName :: Check Text
freshName = liftF $ FreshName id

unifyKinds :: Span -> Kind -> Kind -> Check ()
unifyKinds span k1 k2 = liftF $ UnifyKinds span k1 k2 ()

unifyTypes :: Span -> Type -> Type -> Check (Either Error ())
unifyTypes span t1 t2 = liftF $ UnifyTypes span t1 t2 id

lookupKind :: Text -> Check (Maybe Kind)
lookupKind t = liftF $ LookupKind t id

lookupType :: Text -> Check (Maybe EnvType)
lookupType t = liftF $ LookupType t id

lookupInstanceDict :: DictLookup -> Check (Maybe Text)
lookupInstanceDict c = liftF $ LookupInstanceDict c id

lookupClass :: Text -> Check (Maybe Class)
lookupClass n = liftF $ LookupClass n id

addClass :: Text -> Class -> Check ()
addClass n c = liftF $ AddClass n c ()

addInstance :: Text -> Instance -> Check ()
addInstance c i = liftF $ AddInstance c i ()

getKindSubst :: Check KindSubst
getKindSubst = liftF $ GetKindSubst id

getTypeSubst :: Check TypeSubst
getTypeSubst = liftF $ GetTypeSubst id

getCheckEnv :: Check CheckEnv
getCheckEnv = liftF $ GetCheckEnv id

inExtendedEnv :: CheckEnv -> Check a -> Check a
inExtendedEnv env m = liftF $ InExtendedEnv env m id

inExtendedKindEnv :: Map Text Kind -> Check a -> Check a
inExtendedKindEnv env m = liftF $ InExtendedKindEnv env m id

inExtendedTypeEnv :: Map Text EnvType -> Check a -> Check a
inExtendedTypeEnv env m = liftF $ InExtendedTypeEnv env m id

inExtendedInstanceEnv :: Map DictLookup Text -> Check a -> Check a
inExtendedInstanceEnv env m = liftF $ InExtendedInstanceEnv env m id

retain :: Check a -> Check a
retain m = liftF $ Retain m id

debugEnv :: Check ()
debugEnv = liftF $ DebugEnv ()

debugTypeSubst :: Check ()
debugTypeSubst = liftF $ DebugTypeSubst ()

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

type CheckInterp = StateT CheckState (Except Error)

runCheck :: CheckEnv -> Check a -> Either Error a
runCheck env =
  runExcept .
  flip evalStateT (CheckState env 0 Map.empty Map.empty) .
  iterTM go
  where
  go :: CheckF (CheckInterp a) -> CheckInterp a
  go = \case
    FreshKind reply -> do
      i <- checkCount <+= 1
      reply $ kindUnknown i

    FreshType reply -> do
      i <- checkCount <+= 1
      reply $ typeVar (show i)

    FreshName reply -> do
      i <- checkCount <+= 1
      reply $ show i

    UnifyKinds span k1 k2 next -> unify k1 k2 *> next
      where

      unify :: Kind -> Kind -> CheckInterp ()
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

      bind :: Int -> Kind -> CheckInterp ()
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
      reply result
      where

      unify :: Type -> Type -> ExceptT Error CheckInterp ()
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
          (TypeApp f arg, TypeApp f' arg') -> do
            unify f f'
            unify arg arg'
          (RecordCons f t rest, RecordCons f' t' rest')
            | f == f' -> do
                unify t t'
                unify rest rest'
            | otherwise -> do
                deferredRest <- lift $ iterTM go freshType
                unify rest (recordCons f' t' deferredRest)
                unify rest' (recordCons f t deferredRest)
          (RecordNil, RecordNil) -> pure ()
          _ -> compileError span $
            "Cannot match type `" <>
            prettyType a' <> "` with `" <>
            prettyType b' <> "`."

      bind :: Text -> Type -> ExceptT Error CheckInterp ()
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
      reply res

    LookupType t reply -> do
      res <- use (checkEnv . checkEnvTypes . at t)
      reply res

    LookupInstanceDict c reply -> do
      res <- use (checkEnv . checkEnvInstanceDicts . at c)
      reply res

    LookupClass c reply -> use (checkEnv . checkEnvClasses . at c) >>= reply

    AddClass n c next -> do
      checkEnv . checkEnvClasses . at n .= Just c
      next

    AddInstance c i next -> do
      checkEnv . checkEnvClasses . at c . _Just . _5 %= (i :)
      next

    GetCheckEnv reply -> use checkEnv >>= reply

    GetKindSubst reply -> do
      s <- use checkKindSubst
      reply s

    GetTypeSubst reply -> do
      s <- use checkTypeSubst
      reply s

    InExtendedEnv localEnv m reply -> do
      oldEnv <- use checkEnv
      checkEnv .= unionCheckEnv localEnv oldEnv
      b <- iterTM go m
      checkEnv .= oldEnv
      reply b

    InExtendedKindEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvKinds)
      checkEnv . checkEnvKinds .= localEnv `Map.union` oldEnv
      b <- iterTM go m
      checkEnv . checkEnvKinds .= oldEnv
      reply b

    InExtendedTypeEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvTypes)
      checkEnv . checkEnvTypes .= localEnv `Map.union` oldEnv
      b <- iterTM go m
      checkEnv . checkEnvTypes .= oldEnv
      reply b

    InExtendedInstanceEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvInstanceDicts)
      checkEnv . checkEnvInstanceDicts .= localEnv `Map.union` oldEnv
      b <- iterTM go m
      checkEnv . checkEnvInstanceDicts .= oldEnv
      reply b

    Retain m reply -> do
      old <- get
      b <- iterTM go m
      put old
      reply b

    DebugEnv next -> do
      typeEnv <- use (checkEnv . checkEnvTypes)
      subst <- use checkTypeSubst
      traceM "----"
      for_ (Map.toList typeEnv) $
        \(name, (EnvType (applyTypeSubst subst -> poly) origin)) ->
          traceM $ name <> ": " <> show origin <> " | " <> prettyPolyType poly
      traceM "----"
      next

    DebugTypeSubst next -> do
      subst <- use checkTypeSubst
      traceM "----"
      flip Map.traverseWithKey subst $ \k t ->
        traceM $ k <> " :-> " <> prettyType t
      traceM "----"
      next
