{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
-- |

module Lib.Compiler.Checker where

import           Lib.Prelude              hiding (check)

import           Control.Arrow            ((***))
import           Control.Comonad.Cofree
import           Control.Monad.Trans.Free

import           Data.Functor.Foldable
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set

import           Lib.Compiler.AST
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

check :: Monad m => [SourceAst] -> m ()
check = traverse_ goDecl
  where
  goDecl :: SourceAst -> m ()
  goDecl (span :< (unsafePrj -> decl)) = case decl of
    DataDecl name args constrs -> undefined

--------------------------------------------------------------------------------

type KindEnv = Map Text Kind

type KindSubst = Map Int Kind

substAfter :: KindSubst -> KindSubst -> KindSubst
substAfter s1 s2 = map (applyKindSubst s1) s2 `Map.union` s1

applyKindSubst :: KindSubst -> Kind -> Kind
applyKindSubst sub = cata $ \case
  KindStar -> kindStar
  KindFun f arg -> kindFun f arg
  KindRecord a -> kindRecord a
  KindVar i -> Map.findWithDefault (kindVar i) i sub

data KindInferF a
  = FreshKind (Kind -> a)
  | UnifyKinds Kind Kind a
  | LookupKind Text (Maybe Kind -> a)

deriving instance Functor KindInferF

type KindInfer = FreeT KindInferF (Except Text)

freshKind :: KindInfer Kind
freshKind = liftF $ FreshKind id

unifyKinds :: Kind -> Kind -> KindInfer ()
unifyKinds k1 k2 = liftF $ UnifyKinds k1 k2 ()

lookupKind :: Text -> KindInfer (Maybe Kind)
lookupKind t = liftF $ LookupKind t id

data KindInferState = KindInferState
  { kisCounter     :: Int
  , kisEnv         :: KindEnv
  , kisConstraints :: [(Kind, Kind)]
  }

runKindInfer :: KindEnv -> KindInfer a -> Either Text (a, KindSubst)
runKindInfer env m = runExcept $ do
  (a, res) <- runStateT (iterTM go m) (KindInferState 0 env [])
  subst <- solve $ reverse $ kisConstraints res
  pure (a, subst)
  where
  go
    :: KindInferF (StateT KindInferState (Except Text) a)
    -> StateT KindInferState (Except Text) a
  go = \case
    FreshKind reply -> do
      i <- gets kisCounter
      modify $ \st -> st { kisCounter = kisCounter st + 1 }
      reply $ kindVar i
    UnifyKinds k1 k2 next -> do
      modify $ \st -> st { kisConstraints = (k1, k2) : kisConstraints st}
      next
    LookupKind v reply -> do
      res <- gets (Map.lookup v . kisEnv)
      reply res

  solve :: [(Kind, Kind)] -> Except Text KindSubst
  solve [] = pure Map.empty
  solve ((a, b) : cs) = do
    s <- unify a b
    solve (map (applyKindSubst s *** applyKindSubst s) cs)

  unify :: Kind -> Kind -> Except Text KindSubst
  unify a'@(Fix a) b'@(Fix b) = case (a, b) of
    (KindVar x, _) -> bind x b'
    (_, KindVar x) -> bind x a'
    (KindFun f arg, KindFun f' arg') -> do
      s1 <- unify f f'
      s2 <- unify (applyKindSubst s1 arg) (applyKindSubst s1 arg')
      pure (s2 `substAfter` s1)
    (KindRecord x, KindRecord y) -> unify x y
    _ -> throwError $ "Cannot match kind `" <>
                      prettyKind a' <> "` with `" <> prettyKind b' <> "`."

  bind :: Int -> Kind -> Except Text KindSubst
  bind i k = if i `Set.member` (freeKindVars k)
    then throwError $ "Infinite kind: " <> prettyKind k
    else pure $ Map.singleton i k

  freeKindVars :: Kind -> Set Int
  freeKindVars = cata $ \case
    KindStar -> Set.empty
    KindFun f arg -> f `Set.union` arg
    KindRecord t -> t
    KindVar v -> Set.singleton v

inferKind :: Type -> KindInfer Kind
inferKind = cataM $ \case
  TypeVar v -> lookupKind v >>= \case
    Nothing -> throwError $ "Undefined type variable: " <> v
    Just k -> pure k
  TypeConstructor c -> lookupKind c >>= \case
    Nothing -> throwError $ "Undefined type constructor: " <> c
    Just k -> pure k
  TypeApp kc karg -> do
    kres <- freshKind
    unifyKinds kc (kindFun karg kres)
    pure kres
  RecordCons _ k r -> do
    unifyKinds (kindRecord k) r
    pure r
  RecordNil -> do
    k <- freshKind
    pure (kindRecord k)

--------------------------------------------------------------------------------

checkExpr
  :: (ExprF :<: f, BinderF :<: f)
  => f (m Type) -> m Type
checkExpr = undefined
