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
import Control.Monad.Trans.Maybe
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

typeSubstMerge :: TypeSubst -> TypeSubst -> Maybe TypeSubst
typeSubstMerge s1 s2 = if agree then pure $ s2 `Map.union` s1 else Nothing
  where agree = all (\x -> Map.lookup x s1 == Map.lookup x s2) $
                Map.keys $ s2 `Map.intersection` s1

--------------------------------------------------------------------------------

data CheckF a
  = FreshKind (Kind -> a)
  | FreshType (Type -> a)
  | FreshName (Text -> a)
  | UnifyKinds Span Kind Kind a
  | UnifyTypes Span Type Type (Either Error () -> a)
  | LookupKind Text (Maybe Kind -> a)
  | LookupType Text (Maybe EnvType -> a)
  | LookupInstanceDict Constraint (Maybe Text -> a)
  | LookupClass Text (Maybe Class -> a)
  | AddClass Text Class a
  | AddInstance Text Instance a
  | GetKindSubst (KindSubst -> a)
  | GetTypeSubst (TypeSubst -> a)
  | GetCurrentTypeEnv (Map Text PolyType -> a)
  | forall b. InExtendedKindEnv (Map Text Kind) (Check b) (b -> a)
  | forall b. InExtendedTypeEnv (Map Text EnvType) (Check b) (b -> a)
  | forall b. InExtendedInstanceEnv (Map Constraint Text) (Check b) (b -> a)
  | forall b. Retain (Check b) (b -> a)
  | DebugEnv a

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

lookupInstanceDict :: Constraint -> Check (Maybe Text)
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

getCurrentTypeEnv :: Check (Map Text PolyType)
getCurrentTypeEnv = liftF $ GetCurrentTypeEnv id

inExtendedKindEnv :: Map Text Kind -> Check a -> Check a
inExtendedKindEnv env m = liftF $ InExtendedKindEnv env m id

inExtendedTypeEnv :: Map Text EnvType -> Check a -> Check a
inExtendedTypeEnv env m = liftF $ InExtendedTypeEnv env m id

inExtendedInstanceEnv :: Map Constraint Text -> Check a -> Check a
inExtendedInstanceEnv env m = liftF $ InExtendedInstanceEnv env m id

retain :: Check a -> Check a
retain m = liftF $ Retain m id

debugEnv :: Check ()
debugEnv = liftF $ DebugEnv ()

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

    LookupClass c reply -> do
      res <- use (checkEnv . checkEnvClasses . at c)
      reply res

    AddClass n c next -> do
      checkEnv . checkEnvClasses . at n .= Just c
      next

    AddInstance c i next -> do
      checkEnv . checkEnvClasses . at c . _Just . _5 %= (i :)
      next

    GetCurrentTypeEnv reply -> do
      te <- use (checkEnv . checkEnvTypes)
      reply $ map etPoly te

    GetKindSubst reply -> do
      s <- use checkKindSubst
      reply s

    GetTypeSubst reply -> do
      s <- use checkTypeSubst
      reply s

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
    unifyTypes' span fType (argType --> resultType)
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
      unifyTypes' span resultType eType
      pure ((injFix $ stripAnn binder, e'), cs)
    pure ( case' scrutExpr $ map fst alts'
         , resultType
         , scrutCs <> join (map snd alts') )
  Let bindings rest -> do
    (dict, ds) <- inferDefinitionGroup (map (\(n, e) -> (n, Nothing, e)) bindings)
    let dict' = Map.fromList $ map (view _1 &&& defaultEnvType . view _3) dict
    inExtendedTypeEnv dict' $ do
      (restExpr, restType, cs) <- inferExpr rest
      pure (let' (map (view _1 &&& view _2) dict) restExpr, restType, ds <> cs)
  Accessor e field -> do
    (e', eType, cs) <- inferExpr e
    resultType <- freshType
    tailType <- freshType
    let recordType = typeApp tyRecord $ recordCons field resultType tailType
    unifyTypes' span eType recordType
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
  let
    (expls, impls) = partitionEithers $ flip map bindings $
      \(n, mp, e) -> case mp of
        Nothing -> Right (n, e)
        Just p  -> Left (n, p, e)

  outerEnv <- getCurrentTypeEnv

  -- Put explicit signatures in env
  let
    explDict = Map.fromList $ map (view _1 &&& defaultEnvType . view _2) expls
  inExtendedTypeEnv explDict $ do

  -- Check implicit bindings ---------------------------------------------------
  implDict <- for impls $ \(name, _) -> do
    t <- freshType
    pure (name, EnvType (ForAll [] [] t) Recursive)
  -- First, put fresh polytypes into env
  inExtendedTypeEnv (Map.fromList implDict) $ do

  result <- for impls $ \(name, e@(span :< _)) -> do
    (e', t, cs) <- inferExpr e
    Just (EnvType (ForAll _ _ t') _) <- lookupType name
    unifyTypes' span t t'
    pure (name, e', t, cs)

  s <- getTypeSubst

  let
    fixedImpl = getFtvs (applyTypeSubst s outerEnv)
    genericImpl = let ts = map (applyTypeSubst s . view _3) result in
      Set.unions (map getFtvs ts) Set.\\ fixedImpl
  (deferredImpl, retainedImpl) <-
           split fixedImpl genericImpl
                 (applyTypeSubst s $ join $ map (view _4) result)
  infoImpl <- for result $ \(name, e, t, cs) -> do
    let
      poly = quantify (Set.toList genericImpl)
                      [c | c <- retainedImpl, c `elem` applyTypeSubst s cs]
                      (applyTypeSubst s t)
    pure (name, e, poly)

  -- Check explicit bindings ---------------------------------------------------
  infoExpl <- for expls $ \(name, polyGiven, e@(span :< _)) -> do
    (e', t, cs) <- inferExpr e
    (csGiven, tGiven) <- instantiate polyGiven
    unifyTypes' span t tGiven
    s <- getTypeSubst
    let
      fs = getFtvs (applyTypeSubst s outerEnv)
      cs' = applyTypeSubst s cs
      t' = applyTypeSubst s t
      csGiven' = applyTypeSubst s csGiven
      tGiven' = applyTypeSubst s tGiven
      gs = getFtvs tGiven' `Set.difference` fs
      poly = quantify (Set.toList gs) csGiven' t'
    ps' <- filterM (\c -> not <$> entail csGiven' c) cs'
    (ds, rs) <- split fs gs ps'
    if normalizePoly polyGiven /= normalizePoly poly then
        compileError span $
          "The given type signature is too general. The inferred type is\n`" <>
          prettyPolyType poly <> "` while the given signature was\n`" <>
          prettyPolyType polyGiven <> "`."
      else if not (null rs) then
        compileError span $
          "Not enough constraints given in the type signature. " <>
          "Missing constraints that were inferred: `" <>
          prettyConstraints rs <> "`."
      else
        pure ((name, e', poly), ds)
    
  let allDeferred = deferredImpl <> join (map snd infoExpl)
  let allInfo = infoImpl <> map fst infoExpl

  -- Resolve placeholders
  let recConstrs =
        Map.fromList $ map (\(name, _, ForAll _ cs _) -> (name, cs)) allInfo
  resolvedInfo <- for allInfo $ \(name, e, poly@(ForAll _ cs _)) -> do
    traceM $ name <> " :: " <> prettyPolyType poly
    traceM $ name <> " with placeholders: " <> prettyIntermed e
    s <- getTypeSubst
    let cs' = applyTypeSubst s cs
    dicts <- for cs' $ \p -> (p,) <$> freshName
    inExtendedInstanceEnv (Map.fromList dicts) $ do
      e' <- resolvePlaceholders recConstrs e
      let e'' = foldr (abs . varBinder . snd) e' dicts
      traceM $ name <> " after resolving: " <> prettyIntermed e''
      pure (name, e'', poly)

  pure (resolvedInfo, allDeferred)

inferBinder :: Type -> SourceBinder -> Check (Map Text EnvType)
inferBinder expected (span :< b) = case b of
  VarBinder x ->
    pure $ Map.singleton x $ EnvType (ForAll [] [] expected) Default
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
      unifyTypes' span expected result
      Map.unions <$> zipWithM inferBinder (reverse args) binders
  where
  peelArgs :: Type -> ([Type], Type)
  peelArgs = go []
    where go args (Arrow arg rest) = go (arg:args) rest
          go args rest             = (args, rest)

--------------------------------------------------------------------------------

resolvePlaceholders :: Map Text [Constraint] -> Intermed -> Check Intermed
resolvePlaceholders recConstrs =
    cataM (intermed nothing nothing nothing goPlaceholder nothing)
  where
    goPlaceholder :: PlaceholderF Intermed -> Check Intermed
    goPlaceholder p = do
      s <- getTypeSubst
      case p of
        DictionaryPlaceholder span (IsIn cls (unsafePrjFix -> t)) -> do
          withInstanceDict span (IsIn cls (applyTypeSubst s t)) pure
        MethodPlaceholder span (IsIn cls (unsafePrjFix -> t)) name -> do
          withInstanceDict span (IsIn cls (applyTypeSubst s t)) $ \d ->
            pure $ accessor d name
        RecursiveCallPlaceholder span name -> do
          case Map.lookup name recConstrs of
            Just cs -> do
              let ps = map (dictionaryPlaceholder span . map injFix) cs
              resolvePlaceholders recConstrs $ foldl' app (var name) ps
            Nothing -> pure $ Fix $ inj p

      where
      withInstanceDict span c@(IsIn cls t) f = lookupInstanceDict c >>= \case
        Just dict -> f $ var dict
        Nothing -> case t of
          Fix (TypeConstructor con) ->
            compileError span $
              "No instance of class `" <> cls <>
              "` found for type `" <> con <> "`."
          _ -> pure $ Fix $ inj p

split
  :: Set Text -> Set Text -> [Constraint]
  -> Check ([Constraint], [Constraint])
split fixed generic constraints = do
  -- TODO: Use generic for constraint defaulting
  reduced <- reduce constraints
  pure $ flip partition reduced $ \c ->
      all (`Set.member` fixed) (getFtvs c)
  where
  -- | Removes any constraint that is entailed by the others
  reduce :: [Constraint] -> Check [Constraint]
  reduce = go []
    where
      go ds [] = pure ds
      go ds (c:cs) = entail (ds <> cs) c >>= \case
        True -> go ds cs
        False -> go (c:ds) cs

-- | `entail cs c` is True when c is entailed by cs, i.e. c holds whenever
-- all the constraints in cs hold.
entail :: [Constraint] -> Constraint -> Check Bool
entail cs c = do
    supers <- join <$> traverse bySuper cs
    if c `elem` supers
      then pure True
      else byInst c >>= \case
        Nothing -> pure False
        Just cs' -> and <$> traverse (entail cs) cs'
  where
    -- If a constraint `IsIn Cls t` holds, then it must also hold for all
    -- superclasses.
    bySuper :: Constraint -> Check [Constraint]
    bySuper c@(IsIn cls t) = lookupClass cls >>= \case
      Nothing -> pure [c]
      Just (supers, _, _, _, _) -> do
        css <- for supers $ \s -> bySuper (IsIn s t)
        pure (c : join css)
    -- If c matches an instance, return the instance constraints as subgoals.
    byInst :: Constraint -> Check (Maybe [Constraint])
    byInst c@(IsIn cls _) = lookupClass cls >>= \case
      Nothing -> pure Nothing
      Just (_, _, _, _, insts) ->
        msum <$> traverse tryInst insts
      where
      tryInst (cs', h) = matchConstraint h c >>= \case
        Nothing -> pure Nothing
        Just s -> pure $ Just $ map (applyTypeSubst s) cs'


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
        sigs' <- for (getTypeDecls sigs) $
          \(span', name, poly@(ForAll as cs t)) -> do
            when (length cs > 0) $ compileError span' $
              "Class methods are not allowed to be constrained."
            checkType span' poly
            pure ( name
                 , ForAll (param:as) [IsIn cls $ typeVar param] (stripAnn t) )
        s <- getKindSubst
        addClass cls ( supers'
                     , param
                     , applyKindSubst s paramKind
                     , Map.fromList sigs'
                     , [] )
        pure $ map (id *** (\p -> EnvType p Method)) sigs'
    let classEnv = Map.fromList $ join sigs

    inExtendedTypeEnv classEnv $ do

    -- Check all instance declarations
    let mkName (IsIn cls t) = "$" <> cls <> prettyType t
    instances <- for (getInstanceDecls decls) $
      \(span, (IsIn cls (stripAnn -> t)), cs, vals) -> lookupClass cls >>= \case
        Nothing -> compileError span $
          "Instance class `" <> cls <> "` not defined."
        Just (supers, param, kind, sigs, insts) -> do
          -- TODO: check kind of t
          for_ insts $ \(_, c) -> do
            overlap <- unifyConstraints (IsIn cls t) c
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
          csDict <- for cs $ \(IsIn cls' (stripAnn -> t')) -> do
            n <- freshName
            pure (IsIn cls' t', n)
          let self = (IsIn cls t, mkName $ IsIn cls t)
          result <- inExtendedInstanceEnv (Map.fromList $ self : csDict) $ do
            for (getValueDecls vals) $ \(span, name, args, e) -> do
              (e', t', cs') <- inferExpr (foldr spanAbs e args)
              s <- getTypeSubst
              traceM $ name <> " :: " <> prettyType t'
              traceM $ name <> " constraints: " <> prettyConstraints cs'
              -- TODO: make sure `cs` equals the instance constraints
              traceM $ name <> " before: " <> prettyIntermed e'
              e'' <- resolvePlaceholders Map.empty e'
              traceM $ name <> " after: " <> prettyIntermed e''
              pure (name, e'')
          -- Add to class env
          addInstance cls (map (map stripAnn) cs, IsIn cls t)
          -- Build instance dictionaries
          dict <- compileIntermed $ literal $ RecordLit $
                  Map.fromList $ map (view _1 &&& view _2) result
          pure (IsIn cls t, dict)
    let
      instDicts = Map.fromList $ map (mkName . view _1 &&& view _2) instances
      instEnv = Map.fromList $ map (view _1 &&& mkName . view _1) instances

    -- Check all type signatures
    declaredTypes <- for (getTypeDecls decls) $ \(span, name, poly) -> do
      checkType span poly
      pure (name, poly)

    -- Check types of all value declarations
    result <- inExtendedInstanceEnv instEnv $
      checkValues (map (map stripAnn) $ Map.fromList declaredTypes) decls

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
        , foldr spanAbs e args
        )
    (intermeds, ds) <- inferDefinitionGroup defs
    traceM $ "checkValues deferred: " <> show (length ds)
    pure intermeds

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
    s <- getKindSubst
    let
      kindEnv = map (tidyKind . applyKindSubst s) dTypeDict
      typeEnv = do
        (_, name, args, constrs) <- dataDecls
        let result = foldl typeApp (typeConstructor name) (map typeVar args)
        (cname, cargs) <- constrs
        let ty = foldr (-->) result (stripAnn <$> cargs)
        pure $ (cname, ty)
      polyEnv = map (\(name, t) -> (name, ForAll (Set.toList $ getFtvs t) [] t)) typeEnv
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
    -> [(Span, Text, [SourceAst], SourceAst)]
  getValueDecls = mapMaybe $ \(span :< (unsafePrj -> decl)) -> case decl of
    ValueDecl name binders expr -> Just (span, name, binders, expr)
    _                           -> Nothing

checkFormula :: Formula -> Check (Core.Expr, PolyType)
checkFormula (decls, expr) = do
  (kindEnv, typeEnv, exprs) <- checkModule decls
  inExtendedKindEnv kindEnv $ inExtendedTypeEnv typeEnv $ do
    (i, t, ds) <- inferExpr expr
    c <- compileIntermed i
    let gs = getFtvs t
    -- TODO: check type given by column
    let poly = ForAll (Set.toList gs) [] t
    pure (Core.Let (Map.toList exprs) c, poly)

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

unifyTypes' :: Span -> Type -> Type -> Check ()
unifyTypes' s t1 t2 = either throwError pure =<< unifyTypes s t1 t2

unifyConstraints :: Constraint -> Constraint -> Check Bool
unifyConstraints (IsIn c t) (IsIn c' t')
  | c == c'   = retain $ unifyTypes voidSpan t t' >>= \case
      Left _   -> pure False
      Right () -> pure True
  | otherwise = pure False

--------------------------------------------------------------------------------

-- `matchType t1 t2` tries to find a substitution s such that
-- `apply s t1 == t2`
matchType :: Type -> Type -> Check (Maybe TypeSubst)
matchType t1 t2 = runMaybeT $ match t1 t2
  where
  match :: Type -> Type -> MaybeT Check TypeSubst
  match (Fix a) b'@(Fix b) = case (a, b) of
    (TypeVar x, _) -> pure $ Map.singleton x b'
    (TypeConstructor x, TypeConstructor y) | x == y -> pure $ Map.empty
    (TypeApp f arg, TypeApp f' arg') -> do
      sf <- match f f'
      sarg <- match arg arg'
      hoistMaybe $ typeSubstMerge sf sarg
    (RecordCons f t rest, RecordCons f' t' rest')
      | f == f' -> do
          st <- match t t'
          srest <- match rest rest'
          hoistMaybe $ typeSubstMerge st srest
      | otherwise -> do
          deferredRest <- lift freshType
          srest <- match rest (recordCons f' t' deferredRest)
          srest' <- match rest' (recordCons f t deferredRest)
          hoistMaybe $ typeSubstMerge  srest srest'
    (RecordNil, RecordNil) -> pure $ Map.empty
    _ -> hoistMaybe Nothing
  hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
  hoistMaybe = MaybeT . pure

matchConstraint :: Constraint -> Constraint -> Check (Maybe TypeSubst)
matchConstraint (IsIn c t) (IsIn c' t')
  | c == c'   = matchType t t'
  | otherwise = pure Nothing
