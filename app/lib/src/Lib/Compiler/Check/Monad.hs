{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Lib.Compiler.Check.Monad
  ( module Types
  , runCheck
  ) where

import           Lib.Prelude

import           Control.Lens                   hiding ((:<))

import           Data.Align                     (align)
import           Data.Functor.Foldable
import qualified Data.Map                       as Map
import           Data.These                     (These (..))

import           Lib.Compiler.Check.Error
import           Lib.Compiler.Check.Monad.Types as Types
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

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
          (KindTable, KindTable) -> pure ()
          (KindFun f arg, KindFun f' arg') -> do
            unify f f'
            unify arg arg'
          _ -> foldCheck go $ checkError span $ KindMismatch k1' k2' k1 k2

      bind :: Int -> Kind -> CheckInterpT m ()
      bind i k = do
        -- Occurs check
        flip cata k $ \case
          KindUnknown v | v == i ->
            foldCheck go $ checkError span $ InfiniteKind k1 k2
          _ -> pure ()
        checkKindSubst %= kindSubstAfter (Map.singleton i k)

    UnifyTypes t1 t2 reply -> do
      result <- runExceptT $ unify t1 t2
      pure $ reply result
      where

      unify :: Type -> Type -> ExceptT CheckError (CheckInterpT m) ()
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
          (TypeTable x, TypeTable y) | x == y -> pure ()
          (TypeRecord m, TypeRecord m') -> do
            let unifyField k = \case
                  These t t' -> unify t t'
                  _ -> throwError $ FieldMismatch a' b' k
            void $ Map.traverseWithKey unifyField $ align m m'
          _ -> throwError $ TypeMismatch t1' t2' t1 t2

      bind :: Text -> Type -> ExceptT CheckError (CheckInterpT m) ()
      bind x t = do
        -- Occurs check
        flip cata t $ \case
          TypeVar y | x == y ->
            throwError $ InfiniteType t1 t2
          _ -> pure ()
        checkTypeSubst %= typeSubstAfter (Map.singleton x t)

    LookupKind v reply -> do
      res <- use (checkEnv . checkEnvKinds . at v)
      pure $ reply res

    LookupType x reply -> do
      mt <- use (checkEnv . checkEnvTypes . at x)
      case mt of
        Just t -> pure $ reply $ Just (t, x)
        Nothing -> do
          mOp <- use (checkEnv . checkEnvOperators . at x)
          case mOp of
            Nothing -> pure $ reply Nothing
            Just (alias, _) -> do
              res <- use (checkEnv . checkEnvTypes . at alias)
              case res of
                Nothing -> pure $ reply Nothing
                Just t  -> pure $ reply $ Just (t, alias)

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

    AddOperatorAlias o a f next -> do
      checkEnv . checkEnvOperators . at o .= Just (a, f)
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
      _ <- flip Map.traverseWithKey subst $ \k t ->
        traceM $ k <> " :-> " <> prettyType t
      traceM "--------------"
      pure next

    ThrowError e _ -> do
      throwError e

    CatchError m h reply -> do
      b <- foldCheck go m `catchError` (foldCheck go . h)
      pure $ reply b
