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

import           Lib.Prelude                  hiding (abs)

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
import qualified Lib.Compiler.Core            as Core
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

--------------------------------------------------------------------------------

type KindSubst = Map Int Kind

kindSubstAfter :: KindSubst -> KindSubst -> KindSubst
kindSubstAfter s1 s2 = map (applyKindSubst s1) s2 `Map.union` s1

applyKindSubst :: KindSubst -> Kind -> Kind
applyKindSubst sub = cata $ \case
  KindUnknown i -> Map.findWithDefault (kindUnknown i) i sub
  other         -> Fix other

type TypeSubst = Map Text Type

typeSubstAfter :: TypeSubst -> TypeSubst -> TypeSubst
typeSubstAfter s1 s2 = map (applyTypeSubst s1) s2 `Map.union` s1

applyTypeSubst :: TypeSubst -> Type -> Type
applyTypeSubst sub = cata $ \case
  TypeVar x -> Map.findWithDefault (typeVar x) x sub
  other     -> Fix other

--------------------------------------------------------------------------------

data CheckF a
  = FreshKind (Kind -> a)
  | FreshType (Type -> a)
  | FreshName (Text -> a)
  | UnifyKinds Span Kind Kind a
  | UnifyTypes Span Type Type a
  | LookupKind Text (Maybe Kind -> a)
  | LookupType Text (Maybe PolyType -> a)
  | LookupInstanceDict Constraint (Maybe Text -> a)
  | ApplyCurrentKindSubst Kind (Kind -> a)
  | ApplyCurrentTypeSubst Type (Type -> a)
  | GetFreeContextVars (Set Text -> a)
  | forall b. InExtendedKindEnv (Map Text Kind) (Check b) (b -> a)
  | forall b. InExtendedTypeEnv (Map Text PolyType) (Check b) (b -> a)
  | forall b. InExtendedInstanceEnv (Map Constraint Text) (Check b) (b -> a)

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

unifyTypes :: Span -> Type -> Type -> Check ()
unifyTypes span t1 t2 = liftF $ UnifyTypes span t1 t2 ()

lookupKind :: Text -> Check (Maybe Kind)
lookupKind t = liftF $ LookupKind t id

lookupType :: Text -> Check (Maybe PolyType)
lookupType t = liftF $ LookupType t id

lookupInstanceDict :: Constraint -> Check (Maybe Text)
lookupInstanceDict c = liftF $ LookupInstanceDict c id

applyCurrentKindSubst :: Kind -> Check Kind
applyCurrentKindSubst k = liftF $ ApplyCurrentKindSubst k id

applyCurrentTypeSubst :: Type -> Check Type
applyCurrentTypeSubst t = liftF $ ApplyCurrentTypeSubst t id

getFreeContextVars :: Check (Set Text)
getFreeContextVars = liftF $ GetFreeContextVars id

inExtendedKindEnv :: Map Text Kind -> Check a -> Check a
inExtendedKindEnv env m = liftF $ InExtendedKindEnv env m id

inExtendedTypeEnv :: Map Text PolyType -> Check a -> Check a
inExtendedTypeEnv env m = liftF $ InExtendedTypeEnv env m id

inExtendedInstanceEnv :: Map Constraint Text -> Check a -> Check a
inExtendedInstanceEnv env m = liftF $ InExtendedInstanceEnv env m id

data CheckEnv = CheckEnv
  { _checkEnvTypes         :: Map Text PolyType
  , _checkEnvKinds         :: Map Text Kind
  , _checkEnvInstanceDicts :: Map Constraint Text
  , _checkEnvClasses       :: Map Text Class
  }

primCheckEnv :: CheckEnv
primCheckEnv = CheckEnv primTypeEnv primKindEnv Map.empty Map.empty

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

    UnifyTypes span t1 t2 next -> unify t1 t2 *> next
      where

      unify :: Type -> Type -> CheckInterp ()
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
                deferredRest <- iterTM go freshType
                unify rest (recordCons f' t' deferredRest)
                unify rest' (recordCons f t deferredRest)
          (RecordNil, RecordNil) -> pure ()
          _ -> compileError span $
            "Cannot match type `" <>
            prettyType a' <> "` with `" <>
            prettyType b' <> "`."

      bind :: Text -> Type -> CheckInterp ()
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

    ApplyCurrentKindSubst k reply -> do
      subst <- use checkKindSubst
      reply $ applyKindSubst subst k

    ApplyCurrentTypeSubst t reply -> do
      subst <- use checkTypeSubst
      reply (applyTypeSubst subst t)

    GetFreeContextVars reply -> do
      ctx <- use (checkEnv . checkEnvTypes)
      reply $ getFtvs ctx

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

--------------------------------------------------------------------------------

freshTypeDict :: [Text] -> Check (Map Text Type)
freshTypeDict xs = map Map.fromList $ for xs $ \name -> do
  t <- freshType
  pure (name, t)

freshKindDict :: [Text] -> Check (Map Text Kind)
freshKindDict xs = map Map.fromList $ for xs $ \a -> do
  k <- freshKind
  pure (a, k)

--------------------------------------------------------------------------------

inferKind :: SourceType -> Check Kind
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

inferExpr :: SourceAst -> Check (Intermed, Type)
inferExpr (span :< (unsafePrj -> expr)) = case expr of
  Literal lit -> inferLiteral lit
  Abs (hoistCofree unsafePrj -> binder) e -> do
    argType <- freshType
    binderDict <- inferBinder argType binder
    (e', resultType) <- inExtendedTypeEnv binderDict $ inferExpr e
    pure (abs (injFix $ stripAnn binder) e', argType --> resultType)
  App f arg -> do
    (fExpr, fType) <- inferExpr f
    (argExpr, argType) <- inferExpr arg
    resultType <- freshType
    unifyTypes span fType (argType --> resultType)
    pure (app fExpr argExpr, resultType)
  Var x -> lookupType x >>= \case
    Nothing -> compileError span $ "Variable not in scope: " <> x
    Just poly -> do
      (constraints, t) <- instantiate poly
      let
        typeClassDicts = map (typeClassDict span . map injFix) constraints
        varAppliedToDicts = foldl' app (var x) typeClassDicts
      pure (varAppliedToDicts, t)
  Constructor c -> lookupType c >>= \case
    Nothing -> compileError span $ "Type constructor not in scope: " <> c
    Just poly -> do
      -- Atm type constructors are never constrained
      (_, t) <- instantiate poly
      pure (constructor c, t)
  Case scrutinee alts -> do
    resultType <- freshType
    (scrutExpr, scrutType) <- inferExpr scrutinee
    alts' <- for alts $ \(hoistCofree unsafePrj -> binder, e) -> do
      binderDict <- inferBinder scrutType binder
      (e', eType) <- inExtendedTypeEnv binderDict $ inferExpr e
      unifyTypes span resultType eType
      pure (injFix $ stripAnn binder, e')
    pure (case' scrutExpr alts', resultType)
  Let bindings rest -> do
    dict <- inferDefinitionGroup (map (\(n, e) -> (n, Nothing, e)) bindings)
    inExtendedTypeEnv (Map.fromList $ map (view _1 &&& view _3) dict) $ do
      (restExpr, restType) <- inferExpr rest
      pure (let' (map (view _1 &&& view _2) dict) restExpr, restType)

inferLiteral :: LiteralF SourceAst -> Check (Intermed, Type)
inferLiteral lit = case lit of
  NumberLit n  -> pure (literal $ NumberLit n, tyNumber)
  IntegerLit i -> pure (literal $ IntegerLit i, tyInteger)
  StringLit s  -> pure (literal $ StringLit s, tyString)

inferDefinitionGroup
  :: [(Text, Maybe PolyType, SourceAst)]
  -> Check [(Text, Intermed, PolyType)]
inferDefinitionGroup bindings = do
  dict <- for bindings $ \(name, mPoly, _) -> do
    p <- case mPoly of
      Nothing -> ForAll [] [] <$> freshType
      Just p  -> pure p
    pure (name, p)
  inExtendedTypeEnv (Map.fromList dict) $
    for bindings $ \(name, _, e@(span :< _)) -> do
      (e', t) <- inferExpr e
      Just p <- lookupType name
      (_, t') <- instantiate p
      unifyTypes span t t'
      (cs, poly) <- generalize [] t
      pure (name, e', poly)

inferBinder :: Type -> SourceBinder -> Check (Map Text PolyType)
inferBinder expected (span :< b) = case b of
  VarBinder x -> pure $ Map.singleton x (ForAll [] [] expected)
  ConstructorBinder name binders -> lookupType name >>= \case
    Nothing -> compileError span $ "Type constructor not in scope: " <> name
    Just poly -> do
      (_, t) <- instantiate poly
      let
        (args, result) = peelArgs t
      unless (length args == length binders) $ compileError span $
        "Type constructor `" <> name <>
        "` expects " <> show (length args) <>
        " arguments but was given " <> show (length binders) <> "."
      unifyTypes span expected result
      Map.unions <$> zipWithM inferBinder (reverse args) binders
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
 :: [Constraint]
 -> Type
 -> Check ([Constraint], PolyType)
generalize constraints (applyCurrentTypeSubst -> mt) = do
  t <- mt
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
  reduce :: [Constraint] -> Check [Constraint]
  reduce = go []
    where
      go ds [] = pure ds
      go ds (c:cs) = entailed c (ds <> cs) >>= \case
        True -> go ds cs
        False -> go (c:ds) cs
  entailed :: Constraint -> [Constraint] -> Check Bool
  entailed c cs = lookupInstanceDict c >>= \case
    Just _  -> pure True -- Entailed by an instance that's in scope
    Nothing -> pure $ elem c cs

instantiate :: PolyType -> Check ([Constraint], Type)
instantiate (ForAll as cs t) = do
  pool <- freshTypeDict as
  let
    replace :: Type -> Type
    replace = cata $ \t' -> case t' of
      TypeVar x -> fromMaybe (Fix t') $ Map.lookup x pool
      other     -> Fix other
  pure (map (map replace) cs, replace t)

--------------------------------------------------------------------------------

checkModule
  :: Module
  -> Check ( Map Text Kind
           , Map Text PolyType
           , Map Text Core.Expr )
checkModule decls = do
  -- Check all data declarations and extract the resulting kinds and polytypes
  (kinds, dataPolys) <- checkData
  inExtendedTypeEnv dataPolys $ inExtendedKindEnv kinds $ do
    -- Check all type signatures
    let typeDecls = getTypeDecls decls
    declaredTypes <- for typeDecls $ \(span, name, poly) -> do
      checkType span poly
      pure (name, poly)
    -- Check types of all value declarations
    let
      valueDecls = getValueDecls decls
      defs = flip map valueDecls $ \(_, name, args, e) ->
        ( name
        , map stripAnn <$> Map.lookup name (Map.fromList declaredTypes)
        , foldr spanAbs e (map (hoistCofree inj) args)
        )
    result <- inferDefinitionGroup defs
    -- Collect exports and compile expressions
    exprs <- for result $ \(name, i, _) -> (name, ) <$> compileIntermed i
    let
      valuePolys = map (view _1 &&& view _3) result
      moduleKinds = kinds
      moduleTypes = dataPolys `Map.union` Map.fromList valuePolys
      moduleExprs = Map.fromList exprs
    pure (moduleKinds, moduleTypes, moduleExprs)

  where
  checkType :: Span -> SourcePolyType-> Check ()
  checkType span (ForAll as cs t) = do
    -- TODO: Check constraints
    quantifierDict <- freshKindDict as
    inExtendedKindEnv quantifierDict $ do
      inferred <- inferKind t
      unifyKinds span inferred kindType

  checkData :: Check (Map Text Kind, Map Text PolyType)
  checkData = do
    let dataDecls = getDataDecls decls
    -- TODO: Check for duplicate definitions
    -- Kind checking
    dTypeDict <- freshKindDict $ map (view _2) dataDecls
    inExtendedKindEnv dTypeDict $
      for_ dataDecls $ \(span, name, args, constrs) -> do
        argTypeDict <- freshKindDict args
        inExtendedKindEnv argTypeDict $ do
          for_ constrs $ \(_, cargs) ->
            for cargs $ \carg@(cargSpan :< _) -> do
              argKind <- inferKind carg
              unifyKinds cargSpan kindType argKind
        k <- fromJust <$> lookupKind name
        let argKinds = map (\a -> fromJust $ Map.lookup a argTypeDict) args
        unifyKinds span k (foldr kindFun kindType argKinds)
        pure ()
    -- Collect for env
    kindEnv <- traverse (map tidyKind . applyCurrentKindSubst) dTypeDict
    let
      typeEnv = do
        (_, name, args, constrs) <- dataDecls
        let result = foldl typeApp (typeConstructor name) (map typeVar args)
        (cname, cargs) <- constrs
        let ty = foldr (-->) result (stripAnn <$> cargs)
        pure $ (cname, ty)
    polyEnv <- for typeEnv $ \(name, t) -> do
      (_, poly) <- generalize [] t
      pure (name, poly)
    pure (kindEnv, Map.fromList polyEnv)

  getDataDecls
    :: [SourceAst]
    -> [(Span, Text, [Text], [(Text, [SourceType])])]
  getDataDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    DataDecl name args constrs -> Just (span, name, args, constrs')
      where constrs' = map (id *** map (hoistCofree unsafePrj)) constrs
    _                          -> Nothing

  getTypeDecls
    :: [SourceAst]
    -> [(Span, Text, SourcePolyType)]
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

checkFormula :: Formula -> Check (Core.Expr, PolyType)
checkFormula (decls, expr) = do
  (kindEnv, typeEnv, exprs) <- checkModule decls
  inExtendedKindEnv kindEnv $ inExtendedTypeEnv typeEnv $ do
    (i, t) <- inferExpr expr
    c <- compileIntermed i
    (_, p) <- generalize [] t
    pure (Core.Let (Map.toList exprs) c, p)

--------------------------------------------------------------------------------

replaceTypeClassDicts :: Intermed -> Check Compiled
replaceTypeClassDicts = paraM (intermed goExpr goBinder goType goClass goRef)
  where
  nothing
    :: (Functor f, f :<: CompiledF)
    => f (Intermed, Compiled) -> Check Compiled
  nothing = pure . Fix . inj . map snd
  goExpr = nothing
  goBinder = nothing
  goType _ = error "No type expected"
  goClass = \case
    Constrained cs (e, _) -> do
      dicts <- for cs $ \c -> (unsafeConstraint c,) <$> freshName
      e' <- inExtendedInstanceEnv (Map.fromList dicts) $ replaceTypeClassDicts e
      pure $ foldr abs e' (map (varBinder . snd) dicts)
    TypeClassDict span c -> do
      let c'@(IsIn cls t) = unsafeConstraint c
      lookupInstanceDict c' >>= \case
        Just n -> pure $ var n
        Nothing -> compileError span $
          "Type `" <> prettyType t <> "` does not implement the `" <>
          cls <> "` interface."
    where
    unsafeConstraint = map (unsafePrjFix . fst)
  goRef = nothing

compileIntermed :: Intermed -> Check Core.Expr
compileIntermed i = Core.toCore <$> replaceTypeClassDicts i
