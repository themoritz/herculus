{-# LANGUAGE FlexibleContexts #-}
-- |

module Lib.Compiler.Check where

import           Lib.Prelude                  hiding (abs)

import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as T
import           Control.Lens                 hiding ((:<))
import           Control.Monad.Trans.Maybe

import           Data.Align                   (align)
import           Data.Functor.Foldable
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.List                    (partition)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set
import           Data.These                   (These (..))

import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Check.Error
import           Lib.Compiler.Check.Extract
import           Lib.Compiler.Check.Monad
import qualified Lib.Compiler.Core            as Core
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

freshTypeDict :: [Text] -> Check (Map Text Type)
freshTypeDict xs = map Map.fromList $ for xs $ \name -> do
  t <- freshType
  pure (name, t)

freshKindDict :: [Text] -> Check (Map Text Kind)
freshKindDict xs = map Map.fromList $ for xs $ \a -> do
  k <- freshKind
  pure (a, k)

--------------------------------------------------------------------------------

-- | Infer the kind of a given type.
inferType :: SourceType -> Check Kind
inferType = cataM $ \case
  span T.:< t -> case t of
    TypeVar v -> lookupKind v >>= \case
      Nothing -> checkError span $ UndefinedTypeVariable v
      Just k -> pure k
    TypeConstructor c -> lookupKind c >>= \case
      Nothing -> checkError span $ UndefinedTypeConstructor c
      Just k -> pure k
    TypeApp kc karg -> do
      kres <- freshKind
      unifyKinds span (kindFun karg kres) kc
      pure kres
    TypeTable _ ->
      pure kindTable
    TypeRecord _ ->
      pure kindRecord

--------------------------------------------------------------------------------

-- | Top-down type checking
checkExpr :: Type -> SourceAst -> Check (Intermed, [ConstraintToSolve])
checkExpr t (span :< a) =
  ast undefined (checkExpr' t span) undefined undefined (checkRef' t span) a

checkExpr'
  :: Type -> Span -> ExprF SourceAst
  -> Check (Intermed, [ConstraintToSolve])
checkExpr' t span = \case
  Abs (hoistCofree unsafePrj -> binder) body
    | Arrow argType resultType <- t -> do
        binderDict <- checkBinder argType binder
        (bodyExpr, bodyCs) <-
          inExtendedTypeEnv binderDict $ checkExpr resultType body
        pure (abs (injFix $ stripAnn binder) bodyExpr, bodyCs)

  e@(App _ _) -> do
    checkApp t (span :< inj e)

  Case scrutinee alts -> do
    (scrutExpr, scrutType, scrutCs) <- inferExpr scrutinee
    alts' <- for alts $ \(hoistCofree unsafePrj -> binder, e) -> do
      binderDict <- checkBinder scrutType binder
      (e', cs) <- inExtendedTypeEnv binderDict $ checkExpr t e
      pure ((injFix $ stripAnn binder, e'), cs)
    pure ( case' scrutExpr $ map fst alts'
         , scrutCs <> join (map snd alts') )

  Let definitions rest -> do
    (dict, ds) <- inferDefinitionGroup [] definitions
    inExtendedTypeEnv (envFromDefinitions dict) $ do
      (restExpr, cs) <- checkExpr t rest
      pure ( let' (map (view _1 &&& view _2) dict) restExpr
           , ds <> cs )

  other -> do
    (e, t', cs) <- inferExpr' span other
    unifyTypes' span t t'
    pure (e, cs)

checkRef'
  :: Type -> Span -> RefTextF SourceAst
  -> Check (Intermed, [ConstraintToSolve])
checkRef' t span ref = do
  (e, t', cs) <- inferRef' span ref
  unifyTypes' span t t'
  pure (e, cs)

checkApp :: Type -> SourceAst -> Check (Intermed, [ConstraintToSolve])
checkApp t' = \case
  _ :< ExprPat (App f arg) -> do
    argType <- freshType
    (fExpr, fCs) <- checkApp (argType --> t') f
    (argExpr, argCs) <- checkExpr argType arg
    pure (app fExpr argExpr, fCs <> argCs)
  other ->
    checkExpr t' other

--------------------------------------------------------------------------------

-- | Infer the type of an expression. Also generates a transformed AST that
-- may contain placeholders, and a list of deferred constraints.
inferExpr :: SourceAst -> Check (Intermed, Type, [ConstraintToSolve])
inferExpr (span :< a) =
  ast undefined (inferExpr' span) undefined undefined (inferRef' span) a

inferExpr'
  :: Span -> ExprF SourceAst
  -> Check (Intermed, Type, [ConstraintToSolve])
inferExpr' span = \case
  Literal lit -> inferLiteral' lit

  Abs (hoistCofree unsafePrj -> binder) e -> do
    argType <- freshType
    binderDict <- checkBinder argType binder
    (e', resultType, cs) <- inExtendedTypeEnv binderDict $ inferExpr e
    pure (abs (injFix $ stripAnn binder) e', argType --> resultType, cs)

  App f arg -> do
    (fExpr, fType, fCs) <- inferExpr f
    (argExpr, argType, argCs) <- inferExpr arg
    resultType <- freshType
    unifyTypes' span (argType --> resultType) fType
    pure (app fExpr argExpr, resultType, fCs <> argCs)

  Var x -> lookupType x >>= \case
    Nothing -> checkError span $ UndefinedVariable x
    Just (EnvType poly origin, x') -> do
      (constraints, t) <- instantiate poly
      e <- case origin of
        Default -> do
          let placeholders = map (constrToPlaceholder span) constraints
          pure $ foldl' app (var x') placeholders
        Method -> case constraints of
          [IsIn cls t'] -> pure $ methodPlaceholder span cls (injFix t') x'
          _ -> internalError (Just span) $
            "Expected exactly one interface constraint while looking up " <>
            "method variable `" <> x <> "`."
        Recursive -> pure $ recursiveCallPlaceholder span x'
      pure (e, t, map (span,) constraints)

  Constructor c -> lookupType c >>= \case
    Nothing -> checkError span $ UndefinedConstructor c
    Just (EnvType poly _, _) -> do
      -- Type constructors aren't constrained so no need for placeholders here.
      (_, t) <- instantiate poly
      pure (constructor c, t, [])

  Case scrutinee alts -> do
    resultType <- freshType
    (scrutExpr, scrutType, scrutCs) <- inferExpr scrutinee
    alts' <- for alts $ \(hoistCofree unsafePrj -> binder, e) -> do
      binderDict <- checkBinder scrutType binder
      (e', eType, cs) <- inExtendedTypeEnv binderDict $ inferExpr e
      unifyTypes' span resultType eType
      pure ((injFix $ stripAnn binder, e'), cs)
    pure ( case' scrutExpr $ map fst alts'
         , resultType
         , scrutCs <> join (map snd alts') )

  Let definitions rest -> do
    -- No support for user defined signatures in let expression
    (dict, ds) <- inferDefinitionGroup [] definitions
    inExtendedTypeEnv (envFromDefinitions dict) $ do
      (restExpr, restType, cs) <- inferExpr rest
      pure ( let' (map (view _1 &&& view _2) dict) restExpr
           , restType
           , ds <> cs )

  Access e field@(fieldSpan :< _) -> do
    (e', eType, cs) <- inferExpr e
    (field', fieldType, _) <- inferExpr field
    unifyTypes' fieldSpan tyString fieldType
    resultType <- freshType
    let
      Fix (Literal (StringLit fieldStr)) = unsafePrjFix field'
      fields = Map.singleton fieldStr resultType
      placeholder = accessPlaceholder span (injFix eType)
    pure ( app (app placeholder e') field'
         , resultType
         , (span, HasFields fields eType) : cs )

  Deref e@(eSpan :< _) -> do
    (e', eType, cs) <- inferExpr e
    s <- getTypeSubst
    let eType' = applyTypeSubst s eType
    case eType' of
      Fix (Row (TypeTable (InId i))) -> do
        t <- getTableRecordType i
        pure (e', typeRecord t, cs)
      _ -> checkError eSpan $ ExpectedRowOfTable eType'

inferLiteral'
  :: LiteralF SourceAst -> Check (Intermed, Type, [ConstraintToSolve])
inferLiteral' = \case
  NumberLit n      -> pure (literal $ NumberLit n, tyNumber, [])
  IntegerLit i     -> pure (literal $ IntegerLit i, tyInteger, [])
  StringLit s      -> pure (literal $ StringLit s, tyString, [])
  RecordLit fields -> do
    fields' <- traverse inferExpr fields
    pure ( literal $ RecordLit (map (view _1) fields')
         , tyRecord $ typeRecord $ map (view _2) fields'
         , join $ Map.elems $ map (view _3) fields' )

inferRef'
  :: Span -> RefTextF SourceAst -> Check (Intermed, Type, [ConstraintToSolve])
inferRef' span = \case
  TableRef t -> resolveTableRef t >>= \case
    Nothing -> checkError span $ UnknownTable t
    Just i ->
      pure ( tableRef i
           , typeApp tyList (tyRow $ typeTable $ InId i)
           , [] )
  ColumnRef c -> resolveColumnRef c >>= \case
    Nothing -> checkError span $ UnknownColumn c
    Just (i, dataCol) ->
      pure ( columnRef i
           , typeOfDataType (dataCol ^. dataColType)
           , [] )
  ColumnOfTableRef t c -> resolveColumnOfTableRef t c >>= \case
    Nothing -> checkError span $ UnknownColumnOfTable c t
    Just (ti, ci, dataCol) ->
      pure ( columnOfTableRef ti ci
           , typeApp tyList $ typeOfDataType (dataCol ^. dataColType)
           , [] )

-- | Infer and generalize the types of a list of definitions. Every definition
-- can optionally have a type signature that the inferred type will be checked
-- against.
--
-- Returns the list of transformed ASTs and inferred signatures, as well as a
-- list of deferred constraints.
inferDefinitionGroup
  :: [(Text, SourceAst, PolyType)]
  -> [(Text, SourceAst)]
  -> Check ([(Text, Intermed, PolyType)], [ConstraintToSolve])
inferDefinitionGroup expls impls = do
  -- Put explicit signatures in env
  inExtendedTypeEnv (envFromDefinitions expls) $ do

    (resultImpl, deferredImpl) <- inferImplicitDefinitions impls

    -- Put inferred signatures in env
    inExtendedTypeEnv (envFromDefinitions resultImpl) $ do

      resultExpl <- traverse checkExplicitDefinition expls

      pure ( resultImpl <> map fst resultExpl
           , deferredImpl <> join (map snd resultExpl) )

checkExplicitDefinition
  :: (Text, SourceAst, PolyType)
  -> Check ((Text, Intermed, PolyType), [ConstraintToSolve])
checkExplicitDefinition (name, e@(span :< _), polyGiven) = do
  (csGiven, tGiven) <- instantiate polyGiven
  (e', cs) <- checkExpr tGiven e

  s <- getTypeSubst
  env <- map etPoly . view checkEnvTypes <$> getCheckEnv
  let
    fixed = getFtvs (applyTypeSubst s env)
    cs' = applyTypeSubst s cs
    csGiven' = applyTypeSubst s csGiven
    tGiven' = applyTypeSubst s tGiven
    generic = (getFtvs csGiven' `Set.union` getFtvs tGiven') Set.\\ fixed
    poly = quantify (Set.toList generic) csGiven' tGiven'
  ps' <- filterM (\c -> not <$> entail' csGiven' (snd c)) cs'
  (deferred, retained) <- split fixed generic ps'

  s' <- getTypeSubst
  let poly' = applyTypeSubst s' poly
  if normalizePoly polyGiven /= normalizePoly poly' then
      checkError span $ SignatureTooGeneral poly' polyGiven
    else if not (null retained) then
      checkError span $ SignatureMissingConstraints retained
    else do
      e'' <- resolveConstraints Map.empty e' csGiven'
      pure ((name, e'', poly), deferred)

inferImplicitDefinitions
  :: [(Text, SourceAst)]
  -> Check ([(Text, Intermed, PolyType)], [ConstraintToSolve])
inferImplicitDefinitions defs = do
  -- Fix the type environment outside of the definition group for computing
  -- the list of type variables that are "fixed" within te definition group.
  outerEnv <- map etPoly . view checkEnvTypes <$> getCheckEnv

  -- First, put fresh polytypes into env. These get the `Recursive` tag so
  -- that we can later insert `RecursiveCallPlaceholder`s for them.
  dict <- for defs $ \(name, _) -> do
    t <- freshType
    pure (name, EnvType (ForAll [] [] t) Recursive)
  inExtendedTypeEnv (Map.fromList dict) $ do

    -- Infer
    inferred <- for defs $ \(name, e@(span :< _)) -> do
      (e', t, cs) <- inferExpr e
      -- Unify the inferred type with the fresh type to deal with recursion.
      Just (EnvType (ForAll _ _ t') _, _) <- lookupType name
      unifyTypes' span t' t
      pure (name, e', t, cs)

    -- Calculate deferred and retained constraints.
    s <- getTypeSubst
    let
      ftvsConstr = \case
        -- Don't count vars in ordinary constraints as FTVs for ambiguity
        -- check to kick in.
        IsIn _ _ -> Set.empty
        other -> getFtvs other
      ftvsInferred (_, _, t, cs) =
        getFtvs (applyTypeSubst s t) `Set.union`
        Set.unions (map ftvsConstr (applyTypeSubst s $ map snd cs))
      fixed = getFtvs (applyTypeSubst s outerEnv)
      generic = Set.unions (map ftvsInferred inferred) Set.\\ fixed
      css = applyTypeSubst s $ join $ map (view _4) inferred
    (deferred, retained) <- split fixed generic css

    -- Quantify over retained constraints
    s' <- getTypeSubst
    quantified <- for inferred $ \(name, e, t, _) -> do
      let poly = quantify (Set.toList generic)
                          (applyTypeSubst s' retained)
                          (applyTypeSubst s' t)
      pure (name, e, poly)

    -- Resolve placeholders
    let recConstrs = Map.fromList $
                     map (\(n, _, ForAll _ cs _) -> (n, cs)) quantified
    resolved <- for quantified $ \(name, e, poly@(ForAll _ cs _)) -> do
      e' <- resolveConstraints recConstrs e cs
      pure (name, e', poly)

    pure (resolved, deferred)

checkBinder :: Type -> SourceBinder -> Check (Map Text EnvType)
checkBinder expected (span :< b) = case b of
  VarBinder x ->
    pure $ Map.singleton x $ EnvType (ForAll [] [] expected) Default
  WildcardBinder -> pure $ Map.empty
  ConstructorBinder name binders -> lookupType name >>= \case
    Nothing -> checkError span $ UndefinedConstructor name
    Just (EnvType poly _, _) -> do
      (_, t) <- instantiate poly
      let
        (args, result) = peelArgs t
      unless (length args == length binders) $ checkError span $
        WrongNumberOfConstructorArgs name (length args) (length binders)
      unifyTypes' span expected result
      Map.unions <$> zipWithM checkBinder (reverse args) binders
  where
  peelArgs :: Type -> ([Type], Type)
  peelArgs = go []
    where go args (Arrow arg rest) = go (arg:args) rest
          go args rest             = (args, rest)

--------------------------------------------------------------------------------

-- | Inserts a lambda expression that binds a dictionary for every constraint
-- and then resolves placeholders in the given transformed expression.
resolveConstraints
  :: Map Text [Constraint] -> Intermed -> [Constraint] -> Check Intermed
resolveConstraints recConstrs e cs = do
  s <- getTypeSubst
  dicts <- for (applyTypeSubst s cs) $ \c -> do
    n <- freshName
    let
      t = case c of
        IsIn _ t'      -> t'
        HasFields _ t' -> t'
    v <- flip cata t $ \case
      TypeVar v -> pure v
      TypeApp f _ -> f
      _ -> internalError Nothing $
        "Type `" <> prettyType t <>
        "` in retained constraint `" <> prettyConstraint c <>
        "` not in head-normal form."
    pure $ case c of
      IsIn cls _    -> (ByTypeVar cls v, n)
      HasFields _ _ -> (AccessByTypeVar v, n)
  inExtendedInstanceEnv (Map.fromList dicts) $ do
    e' <- resolvePlaceholders recConstrs e
    -- Order of the constraints in the polytype ensures that order of abs equals
    -- order of app.
    pure $ foldr (abs . varBinder . snd) e' dicts

resolvePlaceholders :: Map Text [Constraint] -> Intermed -> Check Intermed
resolvePlaceholders recConstrs =
  cataM (intermed nothing nothing nothing goPlaceholder nothing)
  where

  goPlaceholder :: PlaceholderF Intermed -> Check Intermed
  goPlaceholder p = case p of
    DictionaryPlaceholder span cls t -> getDict span cls t >>= \case
      -- Not found: placeholder will be resolved in surrounding scope
      Nothing -> pure $ Fix $ inj p
      Just d -> pure d
    MethodPlaceholder span cls t name -> getDict span cls t >>= \case
      -- Not found: placeholder will be resolved in surrounding scope
      Nothing -> pure $ Fix $ inj p
      Just d -> pure $ access d (literal $ StringLit name)
    RecursiveCallPlaceholder span name -> do
      case Map.lookup name recConstrs of
        Just cs -> resolvePlaceholders recConstrs $
          foldl' app (var name) (map (constrToPlaceholder span) cs)
        Nothing -> pure $ Fix $ inj p
    AccessPlaceholder span (unsafePrjFix -> t) -> do
      s <- getTypeSubst
      let Fix t' = applyTypeSubst s t
      case t' of
        TypeVar v -> lookupInstanceDict (AccessByTypeVar v) >>= \case
          -- Not found: placeholder will be resolved in surrounding scope
          Nothing -> pure $ Fix $ inj p
          Just n -> pure $ var n
        Row (TypeTable _) ->
          pure $ abs (varBinder "e") $
                 abs (varBinder "field") $
                 access (deref (var "e")) (var "field")
        Record (TypeRecord _) ->
          pure $ abs (varBinder "e") $
                 abs (varBinder "field") $
                 access (var "e") (var "field")
        _ -> internalError (Just span) $
          "Found type `" <> prettyType (Fix t') <>
          "` when resolving access placeholder."

  getDict :: Span -> Text -> Intermed -> Check (Maybe Intermed)
  getDict span cls (unsafePrjFix -> t) = do
    s <- getTypeSubst
    let t' = applyTypeSubst s t
    flip cata t' $ \case
      TypeVar v -> do
        let go :: Text -> Check (Maybe Intermed)
            go c = lookupInstanceDict (ByTypeVar c v) >>= \case
              Nothing -> do
                subs <- getSubClasses c
                mp <- msum <$> traverse go subs
                pure $ map (\p -> access p (literal $ StringLit c)) mp
              Just dict -> pure $ Just $ var dict
        go cls
      TypeConstructor c -> do
        d <- lookupInstanceDict (ByConstructor cls c) >>= \case
          Nothing -> internalError (Just span) $
            "No instance of class `" <> cls <> "` found for type `" <>
            c <> "`."
          Just dict -> pure $ var dict
        byInst cls t' >>= \case
          Nothing -> internalError (Just span) $
            "Cannot find dict for constraint: " <>
            prettyConstraint (IsIn cls t')
          Just inst -> do
            (cs, t'') <- instantiateInstance inst
            -- Important that t'' is on the left because we want to substitute
            -- the type vars there. TODO: Maybe user matchTypes instead?
            unifyTypes' span t'' t
            let dictPlaceholders =
                  map (constrToPlaceholder span) cs
            Just <$> resolvePlaceholders
                       recConstrs
                       (foldl' app d dictPlaceholders)
      TypeApp f' _ -> f'
      _ -> internalError (Just span) $
        "Found record or row type when resolving placeholder."

split
  :: Set Text -> Set Text -> [ConstraintToSolve]
  -> Check ([ConstraintToSolve], [Constraint])
split fixed generic constraints = do
  reduced <- reduce constraints
  let (deferred, retained) = flip partition reduced $ \c ->
        all (`Set.member` fixed) (getFtvs $ snd c)
  checkAmbiguities (fixed `Set.union` generic) retained
  pure (deferred, map snd retained)

checkAmbiguities
  :: Set Text -> [ConstraintToSolve] -> Check ()
checkAmbiguities vars cs = do
  s <- getTypeSubst
  for_ cs $ \(span, c) -> case c of
    -- Only ordinary class constraints can be ambiguous
    IsIn cls _ -> do
      let ambiguities =
            getFtvs (applyTypeSubst s c) Set.\\ vars
      unless (null ambiguities) $ checkError span $ AmbiguousTypeVar cls
    HasFields _ _ -> pure ()

-- | Removes any constraint that is entailed by the others
reduce :: [ConstraintToSolve] -> Check [ConstraintToSolve]
reduce xs =
    simplifyHasFields xs >>=
    traverse toHeadNormal >>=
    simplify [] . join
  where
    simplify ds [] = pure ds
    simplify ds (c:cs) = entail (ds <> cs) c >>= \case
      True -> simplify ds cs
      False -> simplify (c:ds) cs

-- | `entail cs c` is True when c is entailed by cs, i.e. c holds whenever
-- all the constraints in cs hold.
entail :: [ConstraintToSolve] -> ConstraintToSolve -> Check Bool
entail cs c = entail' (map snd cs) (snd c)

entail' :: [Constraint] -> Constraint -> Check Bool
entail' cs c = do
  supers <- join <$> traverse getSupers cs
  if c `elem` supers
    then pure True -- Superclasses are always entailed by their subclasses
    else case c of
      IsIn cls t -> byInst cls t >>= \case
        Nothing -> pure False
        Just (cs', _, _) -> and <$> traverse (entail' cs) cs'
      HasFields _ _ -> pure False

-- | Recursively generate a list of constraints for all superclasses, including
-- itself.
getSupers :: Constraint -> Check [Constraint]
getSupers c = case c of
  IsIn cls t -> lookupClass cls >>= \case
    Nothing -> pure [c]
    Just (supers, _, _, _, _) -> do
      css <- for supers $ \s -> getSupers (IsIn s t)
      pure (c : join css)
  HasFields _ _ -> pure [c]

-- If c matches an instance, return the instance constraints as subgoals.
byInst :: Text -> Type -> Check (Maybe Instance)
byInst cls t = lookupClass cls >>= \case
  Nothing -> pure Nothing
  Just (_, _, _, _, insts) ->
    msum <$> traverse tryInst insts
  where
  tryInst (cs', cls', t')
    | cls' == cls = matchType t' t >>= \case
        Nothing -> pure Nothing
        Just s ->
          pure $ Just (map (applyTypeSubst s) cs', cls', applyTypeSubst s t')
    | otherwise = pure Nothing

getSubClasses :: Text -> Check [Text]
getSubClasses cls = do
  classes <- view checkEnvClasses <$> getCheckEnv
  pure $ mapMaybe select $ Map.toList classes
  where select (c, (supers, _, _, _, _)) =
          if cls `elem` supers then Just c else Nothing

inHeadNormal :: Type -> Bool
inHeadNormal = cata $ \case
  TypeVar _         -> True
  TypeConstructor _ -> False
  TypeApp f _       -> f
  _                 -> False

toHeadNormal :: ConstraintToSolve -> Check [ConstraintToSolve]
toHeadNormal c'@(span, c) = case c of
  IsIn cls t
    | inHeadNormal t -> pure [c']
    | otherwise      -> byInst cls t >>= \case
        Nothing -> checkError span $ MissingInstance cls t
        Just (cs, _, _) -> join <$> traverse toHeadNormal (map (span,) cs)
  HasFields _ _ -> pure [c']

simplifyHasFields :: [ConstraintToSolve] -> Check [ConstraintToSolve]
simplifyHasFields =
  untilFixedM (\cs -> applyTS =<< mapMaybeM byHasFields cs)
  where
  untilFixedM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
  untilFixedM f old = do
    new <- f old
    if new == old then pure new else untilFixedM f new
  applyTS :: [ConstraintToSolve] -> Check [ConstraintToSolve]
  applyTS cs = do
    s <- getTypeSubst
    pure $ applyTypeSubst s cs

-- This does unification, so after it we need to apply current type subst.
byHasFields :: ConstraintToSolve -> Check (Maybe ConstraintToSolve)
byHasFields c'@(span, c) = case c of
  IsIn _ _ -> pure $ Just c'
  HasFields fields (Fix t) -> do
    case t of
      TypeVar _ -> pure $ Just c'
      Record (TypeRecord m) -> do
        m `checkSubsumes` fields
        pure Nothing
      Row (TypeTable refOrId) ->
        case refOrId of
          InId i -> do
            m <- getTableRecordType i
            m `checkSubsumes` fields
            pure Nothing
          InRef _ -> internalError (Just span)
            "Table ref was not resolved in type."
      _ -> checkError span $ TypeDoesNotHaveFields (Fix t)
  where
    checkSubsumes :: Map Text Type -> Map Text Type -> Check ()
    checkSubsumes big small = void $ Map.traverseWithKey go $ align big small
      where go k = \case
              This _ -> pure ()
              That _ -> checkError span $ MissingField k big
              These t t' -> unifyTypes t' t >>= \case
                Left err -> checkAppendError span err $
                  CheckingSubsumption big small k
                Right () -> pure ()

instantiateInstance :: Instance -> Check ([Constraint], Type)
instantiateInstance (cs, _, t) =
  let as = Set.toList $ getFtvs t <> getFtvs cs in
  instantiate (ForAll as cs t)

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

data CheckResult = CheckResult
  { resultCheckEnv :: CheckEnv
  , resultTermEnv  :: HashMap Core.Ident Core.Expr
  , resultTycons   :: Map Text TyconInfo
  }

checkModule :: Module -> Check CheckResult
checkModule (extractDecls -> decls) = do
  -- Extract all fixity declarations
  for_ (extractFixityDecls decls) $ \ExFixityDecl{..} ->
    addOperatorAlias (getText fOperator) (getText fAlias) fFixity

  -- Check all data declarations and extract the resulting kinds and polytypes
  (kinds, dataPolys, resultTycons) <- checkADTs (extractDataDecls decls)

  -- Put kinds and data constuctors in scope
  inExtendedTypeEnv dataPolys $ inExtendedKindEnv kinds $ do

    -- Check all class declarations
    classEnvs <- for (extractClassDecls decls) checkClassDecl

    -- Put class methods in scope
    inExtendedTypeEnv (Map.unions classEnvs) $ do

      -- Check all instance declarations
      let instanceDecls = extractInstanceDecls decls
      instEnv <- for instanceDecls checkInstanceDecl

      -- Put instance env into scope
      inExtendedInstanceEnv (Map.fromList instEnv) $ do

        let valueDecls = extractValueDecls decls

        -- Check all type signatures
        declaredTypes <- for (extractTypeDecls decls) $ \ExTypeDecl {..} -> do
          let name = getText tName
          poly <- checkTypeDecl tSpan tPolyType
          unless (isJust $ find ((== name) . getText  .vName) valueDecls) $
            checkError (getSpan tName) $ MissingImplementation name
          pure (name, poly)

        -- Check types of all value declarations
        result <- checkValueDecls (Map.fromList declaredTypes) valueDecls

        -- Put value types into scope
        inExtendedTypeEnv (envFromDefinitions result) $ do

          -- Build instance dictionaries
          instDicts <- for instanceDecls buildInstanceDict

          -- Collect exports and compile expressions
          exprs <- for result $ \(name, i, _) -> (name, ) <$> compileIntermed i
          let resultTermEnv = Core.toIdentHashMap $
                Map.fromList exprs `Map.union` Map.fromList instDicts
          resultCheckEnv <- getCheckEnv
          pure CheckResult {..}

cleanToplevelConstraints
  :: [ConstraintToSolve] -> Maybe Type -> Check ()
cleanToplevelConstraints cs mt = do
  s <- getTypeSubst
  typeEnv <- map etPoly . view checkEnvTypes <$> getCheckEnv
  let
    fixed = getFtvs (applyTypeSubst s typeEnv)
    typeFtvs = case mt of
      Nothing -> Set.empty
      Just t  -> getFtvs (applyTypeSubst s t)
    generic = typeFtvs Set.\\ fixed
  (ds, rs) <- split fixed generic (applyTypeSubst s cs)
  let cs' = map snd ds <> rs
  -- There should be no remaining constraints here
  unless (null cs') $ do
    cs'' <- traverse mkConstraintPrintable cs'
    internalError Nothing $
      "Toplevel expression still has the following constraints: " <>
      prettyConstraints cs''
  -- Since split unifies, get substitution again
  case mt of
    Nothing -> pure ()
    Just t -> do
      s' <- getTypeSubst
      let
        t' = applyTypeSubst s' t
        generic' = getFtvs t' Set.\\ fixed
      unless (null generic') $ internalError Nothing $
        "Inferred type `" <> prettyType t' <> "` must not be polymorphic."

checkFormula :: Type -> Formula -> Check Core.Expr
checkFormula tGiven (decls, expr) = do
  CheckResult {..} <- checkModule decls
  inExtendedEnv resultCheckEnv $ do
    (i, cs) <- checkExpr tGiven expr
    cleanToplevelConstraints cs (Just tGiven)
    i' <- resolvePlaceholders Map.empty i
    c <- compileIntermed i'
    pure $ Core.Let (HashMap.toList resultTermEnv) c

-- Only for internal use
inferFormula :: Formula -> Check (Core.Expr, Type)
inferFormula (decls, expr) = do
  CheckResult {..} <- checkModule decls
  inExtendedEnv resultCheckEnv $ do
    (i, t, cs) <- inferExpr expr
    cleanToplevelConstraints cs (Just t)
    i' <- resolvePlaceholders Map.empty i
    c <- compileIntermed i'
    s <- getTypeSubst
    pure (Core.Let (HashMap.toList resultTermEnv) c, applyTypeSubst s t)

checkTypeDecl :: Span -> SourcePolyType -> Check PolyType
checkTypeDecl span (ForAll as cs t) = do
  quantifierDict <- freshKindDict as
  inExtendedKindEnv quantifierDict $ do
    inferred <- inferType t
    unifyKinds span kindType inferred
    traverse_ (checkConstraint span) cs
    ForAll as <$> traverse goConstr cs <*> goType t
  where
  goConstr :: SourceConstraint -> Check Constraint
  goConstr (IsIn cls t') =
    IsIn cls <$> goType t'
  goConstr (HasFields fields t') =
    HasFields <$> (traverse goType fields) <*> goType t'

  goType :: SourceType -> Check Type
  goType t' = do
   flip cata t' $ \case
    span' T.:< tF -> case tF of
      TypeTable refOrId -> case refOrId of
        InRef r -> resolveTableRef r >>= \case
          Nothing -> checkError span' $ UnknownTable r
          Just i -> pure $ typeTable (InId i)
        InId i -> pure $ typeTable (InId i)
      TypeApp f arg -> typeApp <$> f <*> arg
      TypeVar v -> pure $ typeVar v
      TypeConstructor c -> pure $ typeConstructor c
      TypeRecord m -> typeRecord <$> sequence m

checkConstraint :: Span -> SourceConstraint -> Check ()
checkConstraint span (IsIn cls t@(spant :< _)) = lookupClass cls >>= \case
  Nothing -> checkError span $ UndefinedClass cls
  Just (_, _, kind, _, _) -> do
    let t' = stripAnn t
    unless (inHeadNormal t') $ checkError spant $ NoHeadNormalForm t'
    k <- inferType t
    unifyKinds spant kind k
checkConstraint _ (HasFields fields t@(spant :< _)) = do
  k <- inferType t
  unifyKinds spant kindType k
  for_ fields $ \f@(spanf :< _) -> do
    k' <- inferType f
    unifyKinds spanf kindType k'

checkInstanceDecl :: ExInstanceDecl -> Check (DictLookup, Text)
checkInstanceDecl ExInstanceDecl {..} = do
  let (SourceText clsSpan cls, t'@(stripAnn -> t)) = iHead
  lookupClass cls >>= \case
    Nothing -> checkError clsSpan $ UndefinedClass cls
    Just (_, _, kind, sigs, insts) -> do
      -- Check kind of instance type
      constrEnv <- map Map.unions $ for (iHead : iConstraints) $ \(_, ct) ->
        freshKindDict $ Set.toList $ getFtvs $ stripAnn ct
      inExtendedKindEnv constrEnv $ do
        for_ iConstraints $ \(SourceText clsSpan' cls', ct) ->
          checkConstraint clsSpan' (IsIn cls' ct)
        k <- inferType t'
        unifyKinds iSpan kind k
      -- Check for overlapping instances
      for_ insts $ \(_, cls', t'') -> do
        overlap <- unifyConstraints (IsIn cls t) (IsIn cls' t'')
        let tSpan :< _ = t'
        when overlap $ checkError tSpan $ OverlappingInstance t''
      -- Check all methods are implemented and no more
      let implVals = Map.fromList $
                     map (getText . vName &&& getSpan . vName) iMethods
      void $ flip Map.traverseWithKey implVals $ \v span' ->
        case Map.lookup v sigs of
          Nothing -> checkError span' $ UndefinedMethod v cls
          Just _  -> pure ()
      void $ flip Map.traverseWithKey sigs $ \s _ ->
        case Map.lookup s implVals of
          Nothing -> checkError clsSpan $ MissingImplementation s
          Just _  -> pure ()
      -- Add to class env
      addInstance cls
        ( map (\(getText -> cls', ct) -> IsIn cls' (stripAnn ct)) iConstraints
        , cls
        , t )
      -- Check instance head and derive dictionary name for this instance
      headConstructor <- getTypeConstructor iSpan t
      let dictName = "$" <> cls <> headConstructor
      pure (ByConstructor cls headConstructor, dictName)

getTypeConstructor :: Span -> Type -> Check Text
getTypeConstructor span t = flip cata t $ \case
  TypeConstructor c -> pure c
  TypeApp f _ -> f
  _ -> checkError span $ InvalidInstanceHeadType t

buildInstanceDict :: ExInstanceDecl -> Check (Text, Core.Expr)
buildInstanceDict ExInstanceDecl {..} = do
  let (SourceText clsSpan cls, stripAnn -> t) = iHead
  lookupClass cls >>= \case
    Nothing -> internalError (Just clsSpan) $
      "Internal: Instance class `" <> cls <> "` not defined."
    Just (supers, param, _, _, _) -> do
      headConstructor <- getTypeConstructor iSpan t
      -- Check instances for all superclasses are defined and build super dict
      superDicts <- for supers $ \super ->
        lookupInstanceDict (ByConstructor super headConstructor) >>= \case
          Nothing -> checkError clsSpan $ MissingSuperclassInstance super
          Just d -> pure (super, var d)
      -- Create new type variables for the instance constraints and head
      (cs, freshHeadType) <- instantiateInstance
        ( map (\(getText -> cls', stripAnn -> t') -> IsIn cls' t') iConstraints
        , cls, t )
      -- Check the method implementations
      interims <- for iMethods $ \ExValueDecl {..} -> do
        let name = getText vName
        Just (EnvType poly _, _) <- lookupType name
        let ForAll _ _ givenType =
              applyTypeSubst (Map.singleton param freshHeadType) poly
        (e, cs') <- checkExpr givenType vExpr
        -- TODO: make sure `cs'` equals the instance constraints
        pure (name, e)
      -- Check constraints and build dict
      -- Need to apply substitution to the constraints since the new type
      -- vairables might have been unified during checking.
      s <- getTypeSubst
      let sourceCs = zip (applyTypeSubst s cs) (map snd iConstraints)
      csDict <- for sourceCs $ \(IsIn cls' t', t'Span :< _) -> do
        n <- freshName
        v <- flip cata t' $ \case
          TypeVar v -> pure v
          _ -> checkError t'Span $ InvalidInstanceConstraintType t'
        pure (ByTypeVar cls' v, n)
      -- Resolve placeholders
      result <- inExtendedInstanceEnv (Map.fromList csDict) $
        for interims $ \(name, e) -> do
          e' <- resolvePlaceholders Map.empty e
          pure (name, e')
      -- Build instance dictionaries
      let record = literal $ RecordLit $ Map.union
            (Map.fromList superDicts)
            (Map.fromList $ map (view _1 &&& view _2) result)
          interm = foldr abs record $ map (varBinder . snd) csDict
      dict <- compileIntermed interm
      pure ("$" <> cls <> headConstructor, dict)

checkClassDecl :: ExClassDecl -> Check (Map Text EnvType)
checkClassDecl ExClassDecl {..} = do
  let (SourceText clsSpan cls, SourceText _ param) = cHead
  lookupClass cls >>= \case
    Nothing -> pure ()
    Just _ -> checkError clsSpan $ DuplicateClass cls
  supers' <- for cSupers $
    \(SourceText superSpan super, SourceText param'Span param') -> do
      lookupClass super >>= \case
        Nothing ->
          checkError superSpan $ UndefinedClass super
        Just _ -> pure ()
      when (param' /= param) $
        checkError param'Span $ UndefinedTypeVariable param'
      pure super
  paramKind <- freshKind

  inExtendedKindEnv (Map.singleton param paramKind) $ do
    sigs' <- for cMethods $ \ExTypeDecl {..} -> do
      ForAll as cs t <- checkTypeDecl tSpan tPolyType
      when (length cs > 0) $ checkError (getSpan tName) $
        SignatureMustBeUnconstrained (getText tName)
      pure ( getText tName
           , ForAll (param:as) [IsIn cls $ typeVar param] t )
    s <- getKindSubst
    addClass cls ( supers'
                 , param
                 , applyKindSubst s paramKind
                 , Map.fromList sigs'
                 , [] )
    pure $ Map.fromList $ map (id *** (\p -> EnvType p Method)) sigs'

checkADTs
  :: [ExDataDecl]
  -> Check ( Map Text Kind
           , Map Text EnvType
           , Map Text TyconInfo)
checkADTs dataDecls = do
  -- TODO: Check for duplicate definitions
  -- Kind checking
  dTypeDict <- freshKindDict $ map (getText . dName) dataDecls
  inExtendedKindEnv dTypeDict $
    for_ dataDecls $ \ExDataDecl {..} -> do
      let args = map getText dArgs
      argTypeDict <- freshKindDict args
      inExtendedKindEnv argTypeDict $ do
        for_ dConstrs $ \(_, _, cargs) ->
          for cargs $ \carg@(cargSpan :< _) -> do
            argKind <- inferType carg
            unifyKinds cargSpan kindType argKind
      k <- fromJust <$> lookupKind (getText dName)
      let argKinds = map (\a -> fromJust $ Map.lookup a argTypeDict) args
      unifyKinds dSpan k (foldr kindFun kindType argKinds)
  -- Collect for env
  s <- getKindSubst
  let
    kindEnv = map (tidyKind . applyKindSubst s) dTypeDict
    typeEnv = do
      ExDataDecl {..} <- dataDecls
      let result = foldl typeApp (typeConstructor $ getText dName)
                                 (map (typeVar . getText) dArgs)
      (_, getText -> cname, cargs) <- dConstrs
      let ty = foldr (-->) result (stripAnn <$> cargs)
      pure $ (cname, ty)
    quant t = ForAll (Set.toList $ getFtvs t) [] t
    polyEnv = map (id *** quant) typeEnv
    --
    buildTycon ExDataDecl {..} =
      ( getText dName
      , TyconInfo
        { tyconKind = fromJust $ Map.lookup (getText dName) kindEnv
        , tyconParams = map getText dArgs
        , tyconValueConstrs =
            map (\(_, getText -> c, map stripAnn -> cargs) -> (c, cargs)) dConstrs
        }
      )
  pure ( kindEnv
       , map defaultEnvType $ Map.fromList polyEnv
       , Map.fromList $ map buildTycon dataDecls
       )

checkValueDecls
  :: Map Text PolyType
  -> [ExValueDecl]
  -> Check [(Text, Intermed, PolyType)]
checkValueDecls declaredTypes decls = do
  let
    (impls, expls) = partitionEithers $ flip map decls $ \ExValueDecl {..} -> do
      let name = getText vName
      case Map.lookup name declaredTypes of
        Nothing -> Left (name, vExpr)
        Just p  -> Right (name, vExpr, p)
  (intermeds, ds) <- inferDefinitionGroup expls impls
  cleanToplevelConstraints ds Nothing
  pure intermeds

--------------------------------------------------------------------------------

envFromDefinitions :: [(Text, a, PolyType)] -> Map Text EnvType
envFromDefinitions = Map.fromList . map (view _1 &&& defaultEnvType . view _3)

cleanUpIntermed :: Intermed -> Check Compiled
cleanUpIntermed i =
  cataM (intermed goExpr goBinder goType goPlaceholder goRef) i
  where
  goExpr          = nothing
  goBinder        = nothing
  goType _        = internalError Nothing $
    "Encountered type while cleaning up transformed AST:\n" <>
    prettyIntermed i
  goPlaceholder _ = internalError Nothing $
    "Encountered placeholder while cleaning up transformed AST:\n" <>
    prettyIntermed i
  goRef           = nothing

compileIntermed :: Intermed -> Check Core.Expr
compileIntermed i = Core.toCore <$> cleanUpIntermed i

unifyTypes' :: Span -> Type -> Type -> Check ()
unifyTypes' s t1 t2 = either (checkError s) pure =<< unifyTypes t1 t2

unifyConstraints :: Constraint -> Constraint -> Check Bool
unifyConstraints (IsIn c t) (IsIn c' t')
  | c == c'   = retain $ unifyTypes t t' >>= \case
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
    (TypeConstructor x, TypeConstructor y) | x == y -> pure Map.empty
    (TypeApp f arg, TypeApp f' arg') -> do
      sf <- match f f'
      sarg <- match arg arg'
      hoistMaybe $ typeSubstMerge sf sarg
    (TypeTable x, TypeTable y) | x == y -> pure Map.empty
    (TypeRecord m, TypeRecord m') -> do
      let matchField s (_, th) = case th of
            These t t' -> do
              s' <- match t t'
              hoistMaybe $ typeSubstMerge s s'
            _ -> hoistMaybe Nothing
      foldM matchField Map.empty $ Map.toList $ align m m'
    _ -> hoistMaybe Nothing
  hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
  hoistMaybe = MaybeT . pure

matchConstraint :: Constraint -> Constraint -> Check (Maybe TypeSubst)
matchConstraint (IsIn c t) (IsIn c' t')
  | c == c'   = matchType t t'
  | otherwise = pure Nothing
