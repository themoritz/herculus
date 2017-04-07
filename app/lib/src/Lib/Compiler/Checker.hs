{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
-- |

module Lib.Compiler.Checker where

import           Lib.Prelude

import           Control.Arrow                ((&&&), (***))
import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as T
import           Control.Lens                 hiding ((:<))
import           Control.Monad.Trans.Free

import           Data.Functor.Foldable
import           Data.List                    (partition)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

--------------------------------------------------------------------------------

type KindEnv = Map Text Kind

type KindSubst = Map Int Kind

substAfter :: KindSubst -> KindSubst -> KindSubst
substAfter s1 s2 = map (applyKindSubst s1) s2 `Map.union` s1

applyKindSubst :: KindSubst -> Kind -> Kind
applyKindSubst sub = cata $ \case
  KindUnknown i -> Map.findWithDefault (kindUnknown i) i sub
  other -> Fix other

data KindInferF a
  = FreshKind (Kind -> a)
  | UnifyKinds Span Kind Kind a
  | LookupKind Text (Maybe Kind -> a)
  | forall b. WithExtendedKindEnv [(Text, Kind)] (KindInfer b) (b -> a)
  | ApplySubst Kind (Kind -> a)

deriving instance Functor KindInferF

type KindInfer = FreeT KindInferF (Except Error)

freshKind :: KindInfer Kind
freshKind = liftF $ FreshKind id

unifyKinds :: Span -> Kind -> Kind -> KindInfer ()
unifyKinds span k1 k2 = liftF $ UnifyKinds span k1 k2 ()

lookupKind :: Text -> KindInfer (Maybe Kind)
lookupKind t = liftF $ LookupKind t id

withExtendedKindEnv :: [(Text, Kind)] -> KindInfer a -> KindInfer a
withExtendedKindEnv env m = liftF $ WithExtendedKindEnv env m id

applySubst :: Kind -> KindInfer Kind
applySubst k = liftF $ ApplySubst k id

data KindInferState = KindInferState
  { _kisCounter      :: Int
  , _kisEnv          :: KindEnv
  , _kisSubstitution :: KindSubst
  }

makeLenses ''KindInferState

type KindInferInterp = StateT KindInferState (Except Error)

runKindInfer :: KindEnv -> KindInfer a -> Either Error a
runKindInfer env m =
  runExcept (evalStateT (iterTM go m) (KindInferState 0 env Map.empty))
  where

  go :: KindInferF (KindInferInterp a) -> KindInferInterp a
  go = \case
    FreshKind reply -> do
      i <- kisCounter <+= 1
      reply $ kindUnknown i

    UnifyKinds span k1 k2 next -> unify k1 k2 *> next
      where

      unify :: Kind -> Kind -> KindInferInterp ()
      unify k1' k2' = do
        subst <- use kisSubstitution
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

      bind :: Int -> Kind -> KindInferInterp ()
      bind i k = do
        -- Occurs check
        flip cata k $ \case
          KindUnknown v | v == i -> do
            subst <- use kisSubstitution
            let
              k1' = applyKindSubst subst k1
              k2' = applyKindSubst subst k2
            compileError span $
              "Infinite kind while trying to unify `" <>
              prettyKind k1' <> "` with `" <>
              prettyKind k2' <> "`."
          _ -> pure ()
        kisSubstitution %= substAfter (Map.singleton i k)

    LookupKind v reply -> do
      res <- use (kisEnv . at v)
      reply res

    WithExtendedKindEnv localEnv action reply -> do
      oldEnv <- use kisEnv
      kisEnv .= (Map.fromList localEnv) `Map.union` oldEnv
      b <- iterTM go action
      kisEnv .= oldEnv
      reply b

    ApplySubst k reply -> do
      subst <- use kisSubstitution
      reply $ applyKindSubst subst k

freshKindDict :: [Text] -> KindInfer [(Text, Kind)]
freshKindDict as = for as $ \a -> do
  k <- freshKind
  pure (a, k)

inferKind :: SourceType -> KindInfer Kind
inferKind = cataM $ \case
  span T.:< TypeVar v -> lookupKind v >>= \case
    Nothing -> compileError span $ "Undefined type variable: " <> v
    Just k -> pure k
  span T.:< TypeConstructor c -> lookupKind c >>= \case
    Nothing -> compileError span $ "Undefined type constructor: " <> c
    Just k -> pure k
  span T.:< TypeApp kc karg -> do
    kres <- freshKind
    unifyKinds span kc (kindFun karg kres)
    pure kres
  span T.:< RecordCons _ k r -> do
    unifyKinds span (kindRecord k) r
    pure r
  _ T.:< RecordNil -> do
    k <- freshKind
    pure (kindRecord k)

--------------------------------------------------------------------------------

data CheckF a
  = LookupInstanceDict (Constraint Type) (Maybe Text -> a)
  | LookupType Text (Maybe (PolyType Type) -> a)
  | GetFreeContextVars (Set Text -> a)
  | forall b. LiftKindInfer (KindInfer b) (b -> a)
  | forall b. InExtendedKindEnv [(Text, Kind)] (Check b) (b -> a)
  | forall b. InExtendedTypeEnv [(Text, PolyType Type)] (Check b) (b -> a)
  | FreshType (Type -> a)
  | UnifyTypes Span Type Type a

deriving instance Functor CheckF

type Check = FreeT CheckF (Except Error)

lookupInstanceDict :: Constraint Type -> Check (Maybe Text)
lookupInstanceDict c = liftF $ LookupInstanceDict c id

lookupType :: Text -> Check (Maybe (PolyType Type))
lookupType t = liftF $ LookupType t id

getFreeContextVars :: Check (Set Text)
getFreeContextVars = liftF $ GetFreeContextVars id

liftKindInfer :: KindInfer a -> Check a
liftKindInfer m = liftF $ LiftKindInfer m id

inExtendedKindEnv :: [(Text, Kind)] -> Check a -> Check a
inExtendedKindEnv env m = liftF $ InExtendedKindEnv env m id

inExtendedTypeEnv :: [(Text, PolyType Type)] -> Check a -> Check a
inExtendedTypeEnv env m = liftF $ InExtendedTypeEnv env m id

freshType :: Check Type
freshType = liftF $ FreshType id

unifyTypes :: Span -> Type -> Type -> Check ()
unifyTypes span t1 t2 = liftF $ UnifyTypes span t1 t2 ()

data CheckEnv = CheckEnv
  { _checkEnvTypes         :: Map Text (PolyType Type)
  , _checkEnvKinds         :: KindEnv
  , _checkEnvInstanceDicts :: Map (Constraint Type) Text
  }

freshTypeDict :: [Text] -> Check [(Text, Type)]
freshTypeDict = traverse $ \name -> do
  t <- freshType
  pure (name, t)

primCheckEnv :: CheckEnv
primCheckEnv = CheckEnv primTypeEnv primKindEnv Map.empty

makeLenses ''CheckEnv

data CheckState = CheckState
  { _checkEnv   :: CheckEnv
  , _checkCount :: Int
  }

makeLenses ''CheckState

type CheckInterp = StateT CheckState (Except Error)

runCheck :: CheckEnv -> Check a -> Either Error a
runCheck env =
  runExcept .
  flip evalStateT (CheckState env 0) .
  iterTM go
  where
  go :: CheckF (CheckInterp a) -> CheckInterp a
  go = \case
    LookupInstanceDict c reply -> do
      res <- use (checkEnv . checkEnvInstanceDicts . at c)
      reply res
    LookupType t reply -> do
      res <- use (checkEnv . checkEnvTypes . at t)
      reply res
    GetFreeContextVars reply -> do
      ctx <- use (checkEnv . checkEnvTypes)
      reply $ getFtvs ctx
    LiftKindInfer m reply -> do
      kindEnv <- use (checkEnv . checkEnvKinds)
      x <- hoistError $ runKindInfer kindEnv m
      reply x
    InExtendedKindEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvKinds)
      checkEnv . checkEnvKinds .= (Map.fromList localEnv) `Map.union` oldEnv
      b <- iterTM go m
      checkEnv . checkEnvKinds .= oldEnv
      reply b
    InExtendedTypeEnv localEnv m reply -> do
      oldEnv <- use (checkEnv . checkEnvTypes)
      checkEnv . checkEnvTypes .= (Map.fromList localEnv) `Map.union` oldEnv
      b <- iterTM go m
      checkEnv . checkEnvTypes .= oldEnv
      reply b
    FreshType reply -> do
      i <- checkCount <+= 1
      reply $ typeVar (show i)
    UnifyTypes span t1 t2 next -> next

--------------------------------------------------------------------------------

inferType :: SourceAst -> Check Type
inferType (span :< (unsafePrj -> expr)) = case expr of
  Literal lit -> case lit of
    NumberLit _  -> pure $ tyNumber
    IntegerLit _ -> pure $ tyInt
    StringLit _  -> pure $ tyString
  Abs (hoistCofree unsafePrj -> binder) e -> do
    argType <- freshType
    binderDict <- inferBinder argType binder
    resultType <- inExtendedTypeEnv binderDict $ inferType e
    pure $ argType ->: resultType
  App f arg -> do
    fType <- inferType f
    argType <- inferType arg
    resultType <- freshType
    unifyTypes span fType (argType ->: resultType)
    pure resultType
  Var x -> lookupType x >>= \case
    Nothing -> compileError span $ "Variable not in scope: " <> x
    Just poly -> do
      (constraints, t) <- instantiate poly
      pure t
  Constructor x -> lookupType x >>= \case
    Nothing -> compileError span $ "Type constructor not in scope: " <> x
    Just poly -> do
      (constraints, t) <- instantiate poly
      pure t
  Case scrutinee alts -> do
    resultType <- freshType
    scrutType <- inferType scrutinee
    for_ alts $ \(hoistCofree unsafePrj -> binder, e) -> do
      binderDict <- inferBinder scrutType binder
      eType <- inExtendedTypeEnv binderDict $ inferType e
      unifyTypes span resultType eType
    pure resultType

inferBinder :: Type -> SourceBinder -> Check [(Text, PolyType Type)]
inferBinder expected (span :< b) = case b of
  VarBinder x -> pure [(x, ForAll [] [] expected)]
  ConstructorBinder name binders -> lookupType name >>= \case
    Nothing -> compileError span $ "Type constructor not in scope: " <> name
    Just poly -> do
      (_, t) <- instantiate poly
      let
        (args, result) = peelArgs t
      unless (length args == length binders) $ compileError span $
        "Type constructor `" <> name <>
        "` expects " <> show (length args) <>
        "arguments but was given " <> show (length binders) <> "."
      unifyTypes span expected result
      join <$> zipWithM inferBinder (reverse args) binders
  where
  peelArgs :: Type -> ([Type], Type)
  peelArgs = go []
    where go args (Arrow arg rest) = go (arg:args) rest
          go args rest             = (args, rest)

-- | Generalize a type, reduce and retain some constraints.
-- 1. Reduce constraints
-- 2. Let the deferred constraints be those where all ftvs are in the context.
-- 3. Quantify over (ftv retained + ftv t - ftv context)
-- 4. Also put retained constraints into the polytype
generalize
 :: [Constraint Type]
 -> Type
 -> Check ([Constraint Type], PolyType Type)
generalize constraints t = do
  contextVars <- getFreeContextVars
  reduced <- reduce constraints
  let
    (deferred, retained) = flip partition reduced $ \c ->
      all (`Set.member` contextVars) (getFtvs c)
    freeVars =
      (getFtvs retained `Set.union` getFtvs t) `Set.difference` contextVars
  pure (deferred, ForAll (Set.toList freeVars) retained t)
  where
  -- | Removes any constraint that is entailed by the others
  reduce :: [Constraint Type] -> Check [Constraint Type]
  reduce = go []
    where
      go ds [] = pure ds
      go ds (c:cs) = entailed c (ds <> cs) >>= \case
        True -> go ds cs
        False -> go (c:ds) cs
  entailed :: Constraint Type -> [Constraint Type] -> Check Bool
  entailed c cs = lookupInstanceDict c >>= \case
    Just _  -> pure True -- Entailed by an instance that's in scope
    Nothing -> pure $ elem c cs

instantiate :: PolyType Type -> Check ([Constraint Type], Type)
instantiate (ForAll as cs t) = do
  pool <- Map.fromList <$> freshTypeDict as
  let
    replace :: Type -> Type
    replace = cata $ \t' -> case t' of
      TypeVar x -> fromMaybe (Fix t') $ Map.lookup x pool
      other     -> Fix other
  pure (map (map replace) cs, replace t)

--------------------------------------------------------------------------------

checkExpr
  :: (ExprF :<: f, BinderF :<: f)
  => f (m Type) -> m Type
checkExpr = undefined

--------------------------------------------------------------------------------

checkDecls :: [SourceAst] -> Check ()
checkDecls decls = do
  (kinds, types) <- liftKindInfer goData
  for_ kinds $ \(name, k) ->
    traceM $ name <> ": " <> prettyKind k
  polys <- for types $ \(name, t) -> do
    (_, p) <- generalize [] t
    traceM $ name <> ": " <> prettyPolyType p
    pure (name, p)
  inExtendedTypeEnv polys $ inExtendedKindEnv kinds $ do
    let
      typeDecls = getTypeDecls decls
      polys' = map (view _2 &&& map stripAnn . view _3) typeDecls
    for_ typeDecls $ \(span, _, poly) ->
      liftKindInfer $ goType span poly
    inExtendedTypeEnv polys' $ do
      pure ()

  where
  goValues :: Check ()
  goValues = do
    let valueDecls = getValueDecls decls
    pure ()

  goType :: Span -> PolyType SourceType -> KindInfer ()
  goType span (ForAll as cs t) = do
    -- TODO: Check constraints
    quantifierDict <- freshKindDict as
    withExtendedKindEnv quantifierDict $ do
      inferred <- inferKind t
      unifyKinds span inferred kindType

  goData :: KindInfer ([(Text, Kind)], [(Text, Type)])
  goData = do
    let dataDecls = getDataDecls decls
    -- TODO: Check for duplicate definitions
    -- Kind checking
    dTypeDict <- freshKindDict $ map (view _2) dataDecls
    withExtendedKindEnv dTypeDict $
      for_ dataDecls $ \(span, name, args, constrs) -> do
        argTypeDict <- freshKindDict args
        withExtendedKindEnv argTypeDict $ do
          for_ constrs $ \(_, cargs) ->
            for cargs $ \carg@(cargSpan :< _) -> do
              argKind <- inferKind carg
              unifyKinds cargSpan kindType argKind
        k <- fromJust <$> lookupKind name
        let argKinds = map snd argTypeDict
        unifyKinds span k (foldr kindFun kindType argKinds)
        pure ()
    -- Collect for env
    kindEnv <- for dTypeDict $ \(name, k) -> do
      k' <- applySubst k
      pure (name, tidyKind k')
    let
      typeEnv = do
        (_, name, args, constrs) <- dataDecls
        let result = foldl typeApp (typeConstructor name) (map typeVar args)
        (cname, cargs) <- constrs
        let ty = foldr (->:) result (stripAnn <$> cargs)
        pure $ (cname, ty)
    pure (kindEnv, typeEnv)

  getDataDecls
    :: [SourceAst]
    -> [(Span, Text, [Text], [(Text, [SourceType])])]
  getDataDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    DataDecl name args constrs -> Just (span, name, args, constrs')
      where constrs' = map (id *** map (hoistCofree unsafePrj)) constrs
    _                          -> Nothing

  getTypeDecls
    :: [SourceAst]
    -> [(Span, Text, PolyType SourceType)]
  getTypeDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    TypeDecl name poly -> Just (span, name, map (hoistCofree unsafePrj) poly)
    _                  -> Nothing

  getValueDecls
    :: [SourceAst]
    -> [(Span, Text, [SourceBinder], SourceAst)]
  getValueDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    ValueDecl name binders expr -> Just (span, name, binders', expr)
      where binders' = map (hoistCofree unsafePrj) binders
    _                           -> Nothing
