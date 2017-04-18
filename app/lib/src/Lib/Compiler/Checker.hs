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
import           Data.Text                    (pack)

import           Text.Show.Pretty

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

--------------------------------------------------------------------------------

data CheckF a
  = FreshKind (Kind -> a)
  | FreshType (Type -> a)
  | FreshName (Text -> a)
  | UnifyKinds Span Kind Kind a
  | UnifyTypes Span Type Type a
  | LookupKind Text (Maybe Kind -> a)
  | LookupType Text (Maybe EnvType -> a)
  | LookupInstanceDict Constraint (Maybe Text -> a)
  | LookupClass Text (Maybe Class -> a)
  | AddClass Text Class a
  | AddInstance Text Instance a
  | ApplyCurrentKindSubst Kind (Kind -> a)
  | forall b. TypeSubstitutable b => ApplyCurrentTypeSubst b (b -> a)
  | GetFreeContextVars (Set Text -> a)
  | forall b. InExtendedKindEnv (Map Text Kind) (Check b) (b -> a)
  | forall b. InExtendedTypeEnv (Map Text EnvType) (Check b) (b -> a)
  | forall b. InExtendedInstanceEnv (Map Constraint Text) (Check b) (b -> a)
  | forall b. Retain (Check b) (b -> a)

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

lookupType :: Text -> Check (Maybe EnvType)
lookupType t = liftF $ LookupType t id

lookupInstanceDict :: Constraint -> Check (Maybe Text)
lookupInstanceDict c = liftF $ LookupInstanceDict c id

lookupClass :: Text -> Check (Maybe Class)
lookupClass n = liftF $ LookupClass n id

addClass :: Text -> Class -> Check ()
addClass n c = liftF $ AddClass n c ()

addInstance :: Text -> Instance -> Check ()
addInstance c i = liftF $ AddInstance c i ()

applyCurrentKindSubst :: Kind -> Check Kind
applyCurrentKindSubst k = liftF $ ApplyCurrentKindSubst k id

applyCurrentTypeSubst :: TypeSubstitutable a => a -> Check a
applyCurrentTypeSubst t = liftF $ ApplyCurrentTypeSubst t id

getFreeContextVars :: Check (Set Text)
getFreeContextVars = liftF $ GetFreeContextVars id

inExtendedKindEnv :: Map Text Kind -> Check a -> Check a
inExtendedKindEnv env m = liftF $ InExtendedKindEnv env m id

inExtendedTypeEnv :: Map Text EnvType -> Check a -> Check a
inExtendedTypeEnv env m = liftF $ InExtendedTypeEnv env m id

inExtendedInstanceEnv :: Map Constraint Text -> Check a -> Check a
inExtendedInstanceEnv env m = liftF $ InExtendedInstanceEnv env m id

retain :: Check a -> Check a
retain m = liftF $ Retain m id

data Origin
  = Recursive
  | Default
  | Method

data EnvType = EnvType
  { etPoly   :: PolyType
  , etOrigin :: Origin
  }

defaultEnvType :: PolyType -> EnvType
defaultEnvType = flip EnvType Default

data CheckEnv = CheckEnv
  { _checkEnvTypes         :: Map Text EnvType
  , _checkEnvKinds         :: Map Text Kind
  , _checkEnvInstanceDicts :: Map Constraint Text
  , _checkEnvClasses       :: Map Text Class
  }

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

    LookupClass c reply -> do
      res <- use (checkEnv . checkEnvClasses . at c)
      reply res

    AddClass n c next -> do
      checkEnv . checkEnvClasses . at n .= Just c
      next

    AddInstance c i next -> do
      checkEnv . checkEnvClasses . at c . _Just . _5 %= (i :)
      next

    ApplyCurrentKindSubst k reply -> do
      subst <- use checkKindSubst
      reply $ applyKindSubst subst k

    ApplyCurrentTypeSubst t reply -> do
      subst <- use checkTypeSubst
      reply (applyTypeSubst subst t)

    GetFreeContextVars reply -> do
      ctx <- use (checkEnv . checkEnvTypes)
      subst <- use checkTypeSubst
      reply $ getFtvs $ map (applyTypeSubst subst . etPoly) ctx

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

inferExpr :: SourceAst -> Check (Intermed, Type, [Constraint])
inferExpr (span :< (unsafePrj -> expr)) = case expr of
  Literal lit -> inferLiteral lit
  Abs (hoistCofree unsafePrj -> binder) e -> do
    argType <- freshType
    binderDict <- inferBinder argType binder
    (e', resultType, cs) <- inExtendedTypeEnv binderDict $ inferExpr e
    pure (abs (injFix $ stripAnn binder) e', argType --> resultType, cs)
  App f arg -> do
    (fExpr, fType, fCs) <- inferExpr f
    (argExpr, argType, argCs) <- inferExpr arg
    resultType <- freshType
    unifyTypes span fType (argType --> resultType)
    pure (app fExpr argExpr, resultType, fCs <> argCs)
  Var x -> lookupType x >>= \case
    Nothing -> compileError span $ "Variable not in scope: " <> x
    Just (EnvType poly origin) -> do
      (constraints, t) <- instantiate poly
      let
        dictPlaceholders cs = map (dictionaryPlaceholder span . map injFix) cs
        e = case origin of
          Default -> foldl' app (var x) (dictPlaceholders constraints)
          Method -> case constraints of
            c:cs' ->
              foldl' app (methodPlaceholder span (map injFix c) x)
                         (dictPlaceholders cs')
            [] -> error $
              "Expected at least one constraint while looking up " <>
              "method variable `" <> x <> "`."
          Recursive -> recursiveCallPlaceholder span x
      pure (e, t, constraints)
  Constructor c -> lookupType c >>= \case
    Nothing -> compileError span $ "Type constructor not in scope: " <> c
    Just (EnvType poly _) -> do
      -- Atm we assume type constructors are never constrained
      (_, t) <- instantiate poly
      pure (constructor c, t, [])
  Case scrutinee alts -> do
    resultType <- freshType
    (scrutExpr, scrutType, scrutCs) <- inferExpr scrutinee
    alts' <- for alts $ \(hoistCofree unsafePrj -> binder, e) -> do
      binderDict <- inferBinder scrutType binder
      (e', eType, cs) <- inExtendedTypeEnv binderDict $ inferExpr e
      unifyTypes span resultType eType
      pure ((injFix $ stripAnn binder, e'), cs)
    pure ( case' scrutExpr $ map fst alts'
         , resultType
         , join $ map snd alts' )
  Let bindings rest -> do
    dict <- inferDefinitionGroup (map (\(n, e) -> (n, Nothing, e)) bindings)
    let dict' = Map.fromList $ map (view _1 &&& defaultEnvType . view _3) dict
    inExtendedTypeEnv dict' $ do
      (restExpr, restType, cs) <- inferExpr rest
      pure (let' (map (view _1 &&& view _2) dict) restExpr, restType, cs)
  Accessor e field -> do
    (e', eType, cs) <- inferExpr e
    resultType <- freshType
    tailType <- freshType
    let recordType = typeApp tyRecord $ recordCons field resultType tailType
    unifyTypes span eType recordType
    pure (accessor e' field, resultType, cs)

inferLiteral :: LiteralF SourceAst -> Check (Intermed, Type, [Constraint])
inferLiteral lit = case lit of
  NumberLit n      -> pure (literal $ NumberLit n, tyNumber, [])
  IntegerLit i     -> pure (literal $ IntegerLit i, tyInteger, [])
  StringLit s      -> pure (literal $ StringLit s, tyString, [])
  RecordLit fields -> do
    fields' <- traverse inferExpr fields
    let ty = typeApp tyRecord $
             Map.foldrWithKey recordCons recordNil $
             map (view _2) fields'
    pure ( literal $ RecordLit (map (view _1) fields')
         , ty
         , join $ Map.elems $ map (view _3) fields' )

inferDefinitionGroup
  :: [(Text, Maybe PolyType, SourceAst)]
  -> Check ([(Text, Intermed, PolyType)], [Constraint])
inferDefinitionGroup bindings = do
  -- Partition into explicit and implicit bindings
  let (expls, impls) = partitionEithers $ flip map bindings $
        \(n, mp, e) -> case mp of
          Nothing -> Right (n, e)
          Just p  -> Left (n, p, e)
  -- Put explicit signatures in env
  let explDict = Map.fromList $ map (view _1 &&& defaultEnvType . view _2) expls
  inExtendedTypeEnv explDict $ do

  -- Check implicit bindings
  implDict <- for impls $ \(name, _) -> do
    t <- freshType
    pure (name, EnvType (ForAll [] [] t) Recursive)
  -- First, put fresh polytypes into env
  inExtendedTypeEnv (Map.fromList implDict) $ do

  result <- for impls $ \(name, e) -> do
    (e', t, cs) <- inferExpr e
    pure (name, e', t, cs)
  (deferredImpl, retainedImpl) <- split (join $ map (view _4) result)
  fixed <- getFreeContextVars
  let genericImpl =
        Set.unions (map getFtvs $ map (view _3) result) `Set.difference` fixed
  infoImpl <- for result $ \(name, e, t, _) ->
    pure (name, e, quantify genericImpl retainedImpl t)

  -- Check explicit bindings
  infoExpl <- for expls $ \(name, poly, e@(span :< _)) -> do
    (e', t', cs) <- inferExpr e
    cs' <- applyCurrentTypeSubst cs
    (c's', t) <- instantiate poly
    unifyTypes span t t'
    fs <- getFreeContextVars
    t'' <- applyCurrentTypeSubst t'
    let gs = getFtvs t'' `Set.difference` fs
    let poly' = quantify gs c's' t''
    cs'' <- filterM (\c -> not <$> entailed c c's') cs'
    (ds, rs) <- split cs''
    if poly /= poly' then
        compileError span "Signature too general."
      else if not (null rs) then
        compileError span "Context too weak."
      else
        pure ((name, e', poly), ds)
    
  let allDeferred = deferredImpl <> join (map snd infoExpl)
  pure (infoImpl <> map fst infoExpl, allDeferred)

    -- let recConstrs =
    --       Map.fromList $ map (\(name, _, ForAll _ cs _) -> (name, cs)) intermeds
    -- for intermeds $ \(name, e, poly@(ForAll _ cs _)) -> do
    --   traceM $ prettyPolyType poly
    --   dicts <- for cs $ \p -> (p,) <$> freshName
    --   inExtendedInstanceEnv (Map.fromList dicts) $ do
    --     e' <- resolvePlaceholders recConstrs e
    --     pure (name, e', poly)

inferBinder :: Type -> SourceBinder -> Check (Map Text EnvType)
inferBinder expected (span :< b) = case b of
  VarBinder x -> pure $ Map.singleton x $ EnvType (ForAll [] [] expected) Default
  ConstructorBinder name binders -> lookupType name >>= \case
    Nothing -> compileError span $ "Type constructor not in scope: " <> name
    Just (EnvType poly _) -> do
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

--------------------------------------------------------------------------------

resolvePlaceholders :: Map Text [Constraint] -> Intermed -> Check Intermed
resolvePlaceholders recConstrs =
    cataM (intermed goExpr goBinder goType goPlaceholder goRef)
  where
    goExpr = nothing
    goBinder = nothing
    goType = nothing
    goRef = nothing
    goPlaceholder :: PlaceholderF Intermed -> Check Intermed
    goPlaceholder p = case p of
      DictionaryPlaceholder span (IsIn cls (unsafePrjFix -> t)) -> do
        t' <- applyCurrentTypeSubst t
        getInstanceDict span (IsIn cls t')
      MethodPlaceholder span (IsIn cls (unsafePrjFix -> t)) name -> do
        traceM $ "Method " <> cls
        t' <- applyCurrentTypeSubst t
        d <- getInstanceDict span (IsIn cls t')
        pure $ accessor d name
      RecursiveCallPlaceholder span name -> do
        case Map.lookup name recConstrs of
          Just cs -> do
            let ps = map (dictionaryPlaceholder span . map injFix) cs
            resolvePlaceholders recConstrs $ foldl' app (var name) ps
          Nothing -> pure $ Fix $ inj p

      where
      getInstanceDict span c@(IsIn cls t) = lookupInstanceDict c >>= \case
        Just dict -> pure $ var dict
        Nothing -> case t of
          Fix (TypeConstructor c) ->
            compileError span $
              "No instance of class `" <> cls <>
              "` found for type`" <> c <> "`."
          _ -> pure $ Fix $ inj p

-- Reduce constraints
-- Let the deferred constraints be those where all ftvs are in the context.
split :: [Constraint] -> Check ([Constraint], [Constraint])
split constraints = do
  contextVars <- getFreeContextVars
  constraints' <- applyCurrentTypeSubst constraints
  reduced <- reduce constraints'
  pure $ flip partition reduced $ \c ->
      all (`Set.member` contextVars) (getFtvs c)
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

quantify :: Set Text -> [Constraint] -> Type -> PolyType
quantify as cs t = ForAll (Set.toList as) cs t

-- replaceTypeClassDicts :: Intermed -> Check Compiled
-- replaceTypeClassDicts = paraM (intermed goExpr goBinder goType goClass goRef)
--   where
--   nothing
--     :: (Functor f, f :<: CompiledF)
--     => f (Intermed, Compiled) -> Check Compiled
--   nothing = pure . Fix . inj . map snd
--   goExpr = nothing
--   goBinder = nothing
--   goType _ = error "No type expected"
--   goClass = \case
--     Constrained cs (e, _) -> do
--       dicts <- for cs $ \c -> (unsafeConstraint c,) <$> freshName
--       e' <- inExtendedInstanceEnv (Map.fromList dicts) $ replaceTypeClassDicts e
--       pure $ foldr abs e' (map (varBinder . snd) dicts)
--     TypeClassDict span c -> do
--       let c'@(IsIn cls t) = unsafeConstraint c
--       lookupInstanceDict c' >>= \case
--         Just n -> pure $ var n
--         Nothing -> compileError span $
--           "Type `" <> prettyType t <> "` does not implement the `" <>
--           cls <> "` interface."
--     where
--     unsafeConstraint = map (unsafePrjFix . fst)
--   goRef = nothing

--------------------------------------------------------------------------------

checkModule
  :: Module
  -> Check ( Map Text Kind
           , Map Text EnvType
           , Map Text Core.Expr )
checkModule decls = do
  -- Check all data declarations and extract the resulting kinds and polytypes
  (kinds, dataPolys) <- checkData
  inExtendedTypeEnv dataPolys $ inExtendedKindEnv kinds $ do

    -- Check all class declarations
    sigs <- for (getClassDecls decls) $ \(span, (cls, param), supers, sigs) -> do
      lookupClass cls >>= \case
        Nothing -> pure ()
        Just _ -> compileError span $ "Class already defined"
      supers' <- for supers $ \(super, param') -> do
        lookupClass super >>= \case
          Nothing ->
            compileError span $ "Superclass `" <> super <> "` not defined."
          Just _ -> pure ()
        when (param' /= param) $
          compileError span $ "Class parameter `" <> param' <> "` not defined."
        pure super
      paramKind <- freshKind

      inExtendedKindEnv (Map.singleton param paramKind) $ do
        sigs' <- for (getTypeDecls sigs) $ \(span', name, poly) -> do
          checkType span' poly
          pure (name, map stripAnn poly)
        paramKind' <- applyCurrentKindSubst paramKind
        addClass cls (supers', param, paramKind', Map.fromList sigs', [])
        -- By convention we'll add the constraint for the class currently
        -- defined to the front of the constraint list. This is important
        -- when constructing placeholders.
        let addConstraint (ForAll as cs t) =
              ForAll (param:as) ((IsIn cls $ typeVar param):cs) t
        pure $ map (id *** (\p -> EnvType (addConstraint p) Method)) sigs'
    let classEnv = Map.fromList $ join sigs

    -- Check all instance declarations
    instances <- for (getInstanceDecls decls) $ \(span, (IsIn cls (stripAnn -> t)), cs, vals) -> do
      lookupClass cls >>= \case
        Nothing ->
          compileError span $ "Instance class `" <> cls <> "` not defined."
        Just (supers, param, kind, sigs, insts) -> do
          -- TODO: check kind of t
          -- Check for overlapping instances
          for_ insts $ \(_, t') -> do
            overlap <- unifyConstraints span (IsIn cls t) (IsIn cls t')
            when overlap $ compileError span "Overlapping instances"
          -- Check all methods are implemented and no more
          let implVals = Map.fromList $
                         map (view _2 &&& view _1) (getValueDecls vals)
          void $ flip Map.traverseWithKey implVals $ \v span' ->
            case Map.lookup v sigs of
              Nothing -> compileError span' $
                "Method `" <> v <> " is not a method of class `" <> cls <> "`."
              Just _ -> pure ()
          void $ flip Map.traverseWithKey sigs $ \s _ ->
            case Map.lookup s implVals of
              Nothing -> compileError span $
                "Class method `" <> s <> "` not implemented."
              Just _ -> pure ()
          -- Typecheck the method implementations
          let sigs' = map (map (applyTypeSubst (Map.singleton param t))) sigs
          result <- checkValues sigs' vals
          -- Add to class env
          addInstance cls (map (map stripAnn) cs, t)
          -- Build instance dictionaries
          dict <- compileIntermed $ literal $ RecordLit $
                  Map.fromList $ map (view _1 &&& view _2) result
          pure (IsIn cls t, dict)
    let
      mkName (IsIn cls t, _) = "$" <> cls <> prettyType t
      instDicts = Map.fromList $ map (mkName &&& view _2) instances
      instEnv = Map.fromList $ map (view _1 &&& mkName) instances

    -- Check all type signatures
    declaredTypes <- for (getTypeDecls decls) $ \(span, name, poly) -> do
      checkType span poly
      pure (name, poly)

    -- Check types of all value declarations
    result <- inExtendedTypeEnv classEnv $ inExtendedInstanceEnv instEnv $
      checkValues (map (map stripAnn) $ Map.fromList declaredTypes) decls

    for_ result $ \(name, i, _) -> do
      traceM $ name <> ": " <> prettyIntermed i

    -- Collect exports and compile expressions
    exprs <- for result $ \(name, i, _) -> (name, ) <$> compileIntermed i
    let
      valuePolys = map (view _1 &&& defaultEnvType . view _3) result
      moduleKinds = kinds
      moduleTypes = dataPolys `Map.union` Map.fromList valuePolys
      moduleExprs = Map.fromList exprs `Map.union` instDicts
    pure (moduleKinds, moduleTypes, moduleExprs)

  where
  checkValues
    :: Map Text PolyType
    -> [SourceAst]
    -> Check [(Text, Intermed, PolyType)]
  checkValues declaredTypes decls' = do
    let
      defs = flip map (getValueDecls decls') $ \(_, name, args, e) ->
        ( name
        , Map.lookup name declaredTypes
        , foldr spanAbs e (map (hoistCofree inj) args)
        )
    inferDefinitionGroup defs

  checkType :: Span -> SourcePolyType -> Check ()
  checkType span (ForAll as cs t) = do
    -- TODO: Check constraints
    quantifierDict <- freshKindDict as
    inExtendedKindEnv quantifierDict $ do
      inferred <- inferKind t
      unifyKinds span inferred kindType

  checkData :: Check (Map Text Kind, Map Text EnvType)
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
      polyEnv = map (\(name, t) -> (name, quantify (getFtvs t) [] t)) typeEnv
    pure (kindEnv, map defaultEnvType $ Map.fromList polyEnv)

  getDataDecls
    :: [SourceAst]
    -> [(Span, Text, [Text], [(Text, [SourceType])])]
  getDataDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    DataDecl name args constrs -> Just (span, name, args, constrs')
      where constrs' = map (id *** map (hoistCofree unsafePrj)) constrs
    _                          -> Nothing

  getClassDecls
    :: [SourceAst]
    -> [(Span, (Text, Text), [(Text, Text)], [SourceAst])]
  getClassDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    ClassDecl h supers sigs -> Just (span, h, supers, sigs)
    _                       -> Nothing

  getInstanceDecls
    :: [SourceAst]
    -> [(Span, SourceConstraint, [SourceConstraint], [SourceAst])]
  getInstanceDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    InstanceDecl h cs methods -> Just (span, h', cs', methods)
      where h' = map (hoistCofree unsafePrj) h
            cs' = map (map (hoistCofree unsafePrj)) cs
    _                         -> Nothing

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
    (i, t, cs) <- inferExpr expr
    c <- compileIntermed i
    (_, p) <- generalize [] t
    pure (Core.Let (Map.toList exprs) c, p)

--------------------------------------------------------------------------------

nothing
  :: (Functor f, f :<: g)
  => f (Fix g) -> Check (Fix g)
nothing = pure . Fix . inj

cleanUpIntermed :: Intermed -> Check Compiled
cleanUpIntermed = cataM (intermed goExpr goBinder goType goPlaceholder goRef)
  where
  goExpr          = nothing
  goBinder        = nothing
  goType _        = error "No type expected"
  goPlaceholder _ = error "No placeholder expected"
  goRef           = nothing

compileIntermed :: Intermed -> Check Core.Expr
compileIntermed i = Core.toCore <$> cleanUpIntermed i

unifyConstraints :: Span -> Constraint -> Constraint -> Check Bool
unifyConstraints span (IsIn c t) (IsIn c' t')
  | c == c'   = retain (unifyTypes span t t' $> True)
                `catchError`
                \_ -> pure False
  | otherwise = pure False
