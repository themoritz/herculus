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

import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as T
import Control.Monad.Trans.Maybe
import           Control.Lens                 hiding ((:<))

import           Data.Functor.Foldable
import           Data.List                    (partition)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import qualified Lib.Compiler.Core            as Core
import           Lib.Compiler.Checker.Monad
import           Lib.Compiler.Checker.Extract
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

-- | Infer the type of an expression. Also generates a transformed AST that
-- may contain placeholders, and a list of deferred constraints.
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
      e <- case origin of
        Default -> pure $ foldl' app (var x) (dictPlaceholders constraints)
        Method -> case constraints of
          [c] -> pure $ methodPlaceholder span (map injFix c) x
          _ -> internalError (Just span) $
            "Expected at least one constraint while looking up " <>
            "method variable `" <> x <> "`."
        Recursive -> pure $ recursiveCallPlaceholder span x
      pure (e, t, constraints)
  Constructor c -> lookupType c >>= \case
    Nothing -> compileError span $ "Type constructor not in scope: " <> c
    Just (EnvType poly _) -> do
      -- Type constructors are not allowed to be constrained so no need to
      -- build placeholders.
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
  Let definitions rest -> do
    -- No support for user defined signatures in let expression
    (dict, ds) <- inferDefinitionGroup [] definitions
    inExtendedTypeEnv (envFromDefinitions dict) $ do
      (restExpr, restType, cs) <- inferExpr rest
      pure ( let' (map (view _1 &&& view _2) dict) restExpr
           , restType
           , ds <> cs )
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

-- | Infer and generalize the types of a list of definitions. Every definition
-- can optionally have a type signature that the inferred type will be checked
-- against.
--
-- Returns the list of transformed ASTs and inferred signatures, as well as a
-- list of deferred constraints.
inferDefinitionGroup
  :: [(Text, SourceAst, PolyType)]
  -> [(Text, SourceAst)]
  -> Check ([(Text, Intermed, PolyType)], [Constraint])
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
  -> Check ((Text, Intermed, PolyType), [Constraint])
checkExplicitDefinition (name, e@(span :< _), polyGiven) = do
  (e', t, cs) <- inferExpr e
  (csGiven, tGiven) <- instantiate polyGiven
  unifyTypes' span t tGiven

  s <- getTypeSubst
  env <- map etPoly . view checkEnvTypes <$> getCheckEnv
  let
    fixed = getFtvs (applyTypeSubst s env)
    cs' = applyTypeSubst s cs
    t' = applyTypeSubst s t
    csGiven' = applyTypeSubst s csGiven
    tGiven' = applyTypeSubst s tGiven
    generic = getFtvs tGiven' Set.\\ fixed
    poly = quantify (Set.toList generic) csGiven' t'
  ps' <- filterM (\c -> not <$> entail csGiven' c) cs'
  (deferred, retained) <- split fixed generic ps'

  if normalizePoly polyGiven /= normalizePoly poly then
      compileError span $
        "The given type signature is too general. The inferred type is\n`" <>
        prettyPolyType poly <> "` while the given signature was\n`" <>
        prettyPolyType polyGiven <> "`."
    else if not (null retained) then
      compileError span $
        "Not enough constraints given in the type signature. " <>
        "Missing constraints that were inferred: `" <>
        prettyConstraints retained <> "`."
    else do
      e'' <- resolveConstraints Map.empty e' csGiven'
      pure ((name, e'', poly), deferred)

inferImplicitDefinitions
  :: [(Text, SourceAst)]
  -> Check ([(Text, Intermed, PolyType)], [Constraint])
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
    Just (EnvType (ForAll _ _ t') _) <- lookupType name
    unifyTypes' span t t'
    pure (name, e', t, cs)

  -- Calculate deferred and retained constraints.
  s <- getTypeSubst
  let
    inferredTypes = map (applyTypeSubst s . view _3) inferred
    fixed = getFtvs (applyTypeSubst s outerEnv)
    generic = getFtvs inferredTypes Set.\\ fixed
    css = applyTypeSubst s $ join $ map (view _4) inferred 
  (deferred, retained) <- split fixed generic css
                 
  -- Quantify over the intersection of retained and the inferred constraints
  quantified <- for inferred $ \(name, e, t, cs) -> do
    cs' <- reduce (applyTypeSubst s cs)
    let
      rs = [c | c <- retained, c `elem` applyTypeSubst s cs']
      poly = quantify (Set.toList generic) rs (applyTypeSubst s t)
    pure (name, e, poly, rs)

  -- Resolve placeholders
  let recConstrs = Map.fromList $ map (view _1 &&& view _4) quantified
  resolved <- for quantified $ \(name, e, poly, cs) -> do
    e' <- resolveConstraints recConstrs e cs
    pure (name, e', poly)

  pure (resolved, deferred)

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

-- | Inserts a lambda expression that binds a dictionary for every constraint
-- and then resolves placeholders in the given transformed expression.
resolveConstraints
  :: Map Text [Constraint] -> Intermed -> [Constraint] -> Check Intermed
resolveConstraints recConstrs e cs = do
  s <- getTypeSubst
  dicts <- for (applyTypeSubst s cs) $ \(IsIn cls t) -> do
    n <- freshName
    flip cata t $ \case
      TypeVar v -> pure (ByTypeVar cls v, n)
      TypeApp f _ -> f
      _ -> internalError Nothing $
        "Type in retained constraint not in head-normal form."
  inExtendedInstanceEnv (Map.fromList dicts) $ do
    e' <- resolvePlaceholders recConstrs e
    pure $ foldr (abs . varBinder . snd) e' dicts

resolvePlaceholders :: Map Text [Constraint] -> Intermed -> Check Intermed
resolvePlaceholders recConstrs =
  cataM (intermed nothing nothing nothing goPlaceholder nothing)
  where

  goPlaceholder :: PlaceholderF Intermed -> Check Intermed
  goPlaceholder p = case p of
    DictionaryPlaceholder span c -> getDict span c >>= \case
      -- Not found: placeholder will be resolved in surrounding scope
      Nothing -> pure $ Fix $ inj p
      Just d -> pure d
    MethodPlaceholder span c name -> getDict span c >>= \case
      -- Not found: placeholder will be resolved in surrounding scope
      Nothing -> pure $ Fix $ inj p
      Just d -> pure $ accessor d name
    RecursiveCallPlaceholder span name -> do
      case Map.lookup name recConstrs of
        Just cs -> do
          let ps = map (dictionaryPlaceholder span . map injFix) cs
          resolvePlaceholders recConstrs $ foldl' app (var name) ps
        Nothing -> pure $ Fix $ inj p

  getDict :: Span -> ConstraintF Intermed -> Check (Maybe Intermed)
  getDict span (IsIn cls (unsafePrjFix -> t)) = do
    s <- getTypeSubst
    let t' = applyTypeSubst s t
    flip cata t' $ \case
      TypeVar v -> do
        let go :: Text -> Check (Maybe Intermed)
            go c = lookupInstanceDict (ByTypeVar c v) >>= \case
              Nothing -> do
                subs <- getSubClasses cls
                mp <- msum <$> traverse go subs
                pure $ map (\p -> accessor p c) mp
              Just dict -> pure $ Just $ var dict
        go cls
      TypeConstructor c -> do
        d <- lookupInstanceDict (ByConstructor cls c) >>= \case
          Nothing -> compileError span $
            "No instance of class `" <> cls <> "` found for type `" <>
            c <> "`."
          Just dict -> pure $ var dict
        byInst (IsIn cls t') >>= \case
          Nothing -> compileError span $
            "Internal? Cannot find dict for constraint: " <>
            prettyConstraint (IsIn cls t')
          Just inst -> do
            (cs, t'') <- instantiateInstance inst
            unifyTypes' span t'' t
            let dictPlaceholders =
                  map (dictionaryPlaceholder span . map injFix) cs
            Just <$> resolvePlaceholders
                       recConstrs
                       (foldl' app d dictPlaceholders)
      TypeApp f' _ -> f'
      _ -> compileError span $
        "Internal? Found record type when resolving placeholder."

split
  :: Set Text -> Set Text -> [Constraint]
  -> Check ([Constraint], [Constraint])
split fixed generic constraints = do
  -- TODO: Use generic for constraint defaulting
  reduced <- reduce constraints
  pure $ flip partition reduced $ \c ->
        all (`Set.member` fixed) (getFtvs c)

-- | Removes any constraint that is entailed by the others
reduce :: [Constraint] -> Check [Constraint]
reduce xs = simplify [] =<< join <$> traverse toHeadNormal xs
  where
    simplify ds [] = pure ds
    simplify ds (c:cs) = entail (ds <> cs) c >>= \case
      True -> simplify ds cs
      False -> simplify (c:ds) cs

-- | `entail cs c` is True when c is entailed by cs, i.e. c holds whenever
-- all the constraints in cs hold.
entail :: [Constraint] -> Constraint -> Check Bool
entail cs c = do
    supers <- join <$> traverse getSupers cs
    if c `elem` supers
      then pure True -- Superclasses are always entailed by their subclasses
      else byInst c >>= \case
        Nothing -> pure False
        Just (cs', _) -> and <$> traverse (entail cs) cs'

-- | Recursively generate a list of constraints for all superclasses.
getSupers :: Constraint -> Check [Constraint]
getSupers c@(IsIn cls t) = lookupClass cls >>= \case
  Nothing -> pure [c]
  Just (supers, _, _, _, _) -> do
    css <- for supers $ \s -> getSupers (IsIn s t)
    pure (c : join css)

-- If c matches an instance, return the instance constraints as subgoals.
byInst :: Constraint -> Check (Maybe Instance)
byInst c@(IsIn cls _) = lookupClass cls >>= \case
  Nothing -> pure Nothing
  Just (_, _, _, _, insts) ->
    msum <$> traverse tryInst insts
  where
  tryInst (cs', h) = matchConstraint h c >>= \case
    Nothing -> pure Nothing
    Just s -> pure $ Just (map (applyTypeSubst s) cs', applyTypeSubst s h)

-- TODO: make this more efficient
getSubClasses :: Text -> Check [Text]
getSubClasses cls = do
  classes <- view checkEnvClasses <$> getCheckEnv
  pure $ mapMaybe select $ Map.toList classes
  where select (c, (supers, _, _, _, _)) =
          if cls `elem` supers then Just c else Nothing

inHeadNormal :: Constraint -> Bool
inHeadNormal (IsIn _ t) = flip cata t $ \case
  TypeVar _         -> True
  TypeConstructor _ -> False
  TypeApp f _       -> f
  _                 -> False

toHeadNormal :: Constraint -> Check [Constraint]
toHeadNormal c
  | inHeadNormal c = pure [c]
  | otherwise      = byInst c >>= \case
      Nothing -> compileError voidSpan $
        "Cannot solve the constraint `" <>
        prettyConstraint c <> "`."
      Just (cs, _) -> join <$> traverse toHeadNormal cs

instantiateInstance :: Instance -> Check ([Constraint], Type)
instantiateInstance (cs, IsIn _ t) =
  instantiate (ForAll (Set.toList $ getFtvs t) cs t)

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
  :: CheckEnv -> Module
  -> Either Error (CheckEnv, Map Text Core.Expr)
checkModule env decls = runCheck env $ checkModule' decls

checkModule' :: Module -> Check (CheckEnv, Map Text Core.Expr)
checkModule' decls = do
  -- Check all data declarations and extract the resulting kinds and polytypes
  (kinds, dataPolys) <- checkADTs (extractDataDecls decls)

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

  -- Check all type signatures
  declaredTypes <- for (extractTypeDecls decls) $ \ExTypeDecl {..} -> do
    checkTypeDecl tSpan tPolyType
    pure (tName, tPolyType)

  -- Check types of all value declarations
  result <- checkValueDecls (map (map stripAnn) $ Map.fromList declaredTypes)
                            (extractValueDecls decls)

  -- Put value types into scope
  inExtendedTypeEnv (envFromDefinitions result) $ do

  -- Build instance dictionaries
  instDicts <- for instanceDecls buildInstanceDict

  debugTypeSubst

  -- Collect exports and compile expressions
  exprs <- for result $ \(name, i, _) -> (name, ) <$> compileIntermed i
  let moduleExprs = Map.fromList exprs `Map.union` Map.fromList instDicts
  env <- getCheckEnv
  pure (env, moduleExprs)

checkFormula
  :: CheckEnv -> Formula
  -> Either Error (Core.Expr, PolyType)
checkFormula env f = runCheck env $ checkFormula' f

checkFormula' :: Formula -> Check (Core.Expr, PolyType)
checkFormula' (decls, expr) = do
  (env, exprs) <- checkModule' decls
  inExtendedEnv env $ do
    (i, t, ds) <- inferExpr expr
    i' <- resolvePlaceholders Map.empty i
    c <- compileIntermed i'
    let gs = getFtvs t
    -- TODO: check that there are no deferred constraints
    -- TODO: check type given by column
    let poly = ForAll (Set.toList gs) [] t
    pure (Core.Let (Map.toList exprs) c, poly)

checkTypeDecl :: Span -> SourcePolyType -> Check ()
checkTypeDecl span (ForAll as cs t) = do
  -- TODO: Check constraints
  quantifierDict <- freshKindDict as
  inExtendedKindEnv quantifierDict $ do
    inferred <- inferType t
    unifyKinds span inferred kindType

checkInstanceDecl :: ExInstanceDecl -> Check (DictLookup, Text)
checkInstanceDecl ExInstanceDecl {..} = do
  let IsIn cls t'@(stripAnn -> t) = iHead
  lookupClass cls >>= \case
    Nothing -> compileError iSpan $
      "Instance class `" <> cls <> "` not defined."
    Just (supers, param, kind, sigs, insts) -> do
      -- Check kind of instance type
      k' <- inferType t'
      let (s :< _) = t' in unifyKinds s k' kind
      -- Check for overlapping instances
      for_ insts $ \(_, c) -> do
        overlap <- unifyConstraints (IsIn cls t) c
        when overlap $ compileError iSpan "Overlapping instances"
      -- Check all methods are implemented and no more
      let implVals = Map.fromList $
                     map (vName &&& vSpan) iMethods
      void $ flip Map.traverseWithKey implVals $ \v span' ->
        case Map.lookup v sigs of
          Nothing -> compileError span' $
            "Method `" <> v <> " is not a method of class `" <> cls <> "`."
          Just _ -> pure ()
      void $ flip Map.traverseWithKey sigs $ \s _ ->
        case Map.lookup s implVals of
          Nothing -> compileError iSpan $
            "Class method `" <> s <> "` not implemented."
          Just _ -> pure ()
      -- Add to class env
      addInstance cls (map (map stripAnn) iConstraints, IsIn cls t)
      -- Check instance head and derive dictionary name for this instance
      headConstructor <- getTypeConstructor iSpan t
      let dictName = "$" <> cls <> headConstructor
      pure (ByConstructor cls headConstructor, dictName)

getTypeConstructor :: Span -> Type -> Check Text
getTypeConstructor span = cata $ \case
  TypeConstructor c -> pure c
  TypeApp f _ -> f
  _ -> compileError span $ "Instance head must be type constructor."

buildInstanceDict :: ExInstanceDecl -> Check (Text, Core.Expr)
buildInstanceDict ExInstanceDecl {..} = do
  let IsIn cls (stripAnn -> t) = iHead
  lookupClass cls >>= \case
    Nothing -> compileError iSpan $
      "Internal: Instance class `" <> cls <> "` not defined."
    Just (supers, param, _, _, _) -> do
      headConstructor <- getTypeConstructor iSpan t
      -- Check instances for all superclasses are defined and build super dict
      superDicts <- for supers $ \super ->
        lookupInstanceDict (ByConstructor super headConstructor) >>= \case
          Nothing -> compileError iSpan $
            "No instance for superclass `" <> super <> "` implemented."
          Just d -> pure (super, var d)
      -- Check constraints
      csDict <- for iConstraints $ \(IsIn cls' (stripAnn -> t')) -> do
        n <- freshName
        v <- flip cata t' $ \case
          TypeVar v -> pure v
          _ -> compileError iSpan $
            "Type of instance constraint must be type variable."
        pure (ByTypeVar cls' v, n)
      result <- inExtendedInstanceEnv (Map.fromList csDict) $ do
        for iMethods $ \ExValueDecl {..} -> do
          (e', t', cs) <- inferExpr vExpr
          Just (EnvType (ForAll _ _ givenT) _) <- lookupType vName
          unifyTypes' vSpan t' (applyTypeSubst (Map.singleton param t) givenT)
          -- TODO: make sure `cs` equals the instance constraints
          e'' <- resolvePlaceholders Map.empty e'
          pure (vName, e'')
      -- Build instance dictionaries
      let record = literal $ RecordLit $ Map.union
            (Map.fromList superDicts)
            (Map.fromList $ map (view _1 &&& view _2) result)
          interm = foldr abs record $ map (varBinder . snd) csDict
      dict <- compileIntermed interm
      pure ("$" <> cls <> headConstructor, dict)

checkClassDecl :: ExClassDecl -> Check (Map Text EnvType)
checkClassDecl ExClassDecl {..} = do
  let (cls, param) = cHead
  lookupClass cls >>= \case
    Nothing -> pure ()
    Just _ -> compileError cSpan $ "Class already defined"
  supers' <- for cSupers $ \(super, param') -> do
    lookupClass super >>= \case
      Nothing ->
        compileError cSpan $ "Superclass `" <> super <> "` not defined."
      Just _ -> pure ()
    when (param' /= param) $
      compileError cSpan $ "Class parameter `" <> param' <> "` not defined."
    pure super
  paramKind <- freshKind

  inExtendedKindEnv (Map.singleton param paramKind) $ do
    sigs' <- for cMethods $ \ExTypeDecl {..} -> do
      let ForAll as cs t = tPolyType
      when (length cs > 0) $ compileError tSpan $
        "Class methods are not allowed to be constrained."
      checkTypeDecl tSpan tPolyType
      pure ( tName
           , ForAll (param:as) [IsIn cls $ typeVar param] (stripAnn t) )
    s <- getKindSubst
    addClass cls ( supers'
                 , param
                 , applyKindSubst s paramKind
                 , Map.fromList sigs'
                 , [] )
    pure $ Map.fromList $ map (id *** (\p -> EnvType p Method)) sigs'

checkADTs :: [ExDataDecl] -> Check (Map Text Kind, Map Text EnvType)
checkADTs dataDecls = do
  -- TODO: Check for duplicate definitions
  -- Kind checking
  dTypeDict <- freshKindDict $ map dName dataDecls
  inExtendedKindEnv dTypeDict $
    for_ dataDecls $ \ExDataDecl {..} -> do
      argTypeDict <- freshKindDict dArgs
      inExtendedKindEnv argTypeDict $ do
        for_ dConstrs $ \(_, cargs) ->
          for cargs $ \carg@(cargSpan :< _) -> do
            argKind <- inferType carg
            unifyKinds cargSpan kindType argKind
      k <- fromJust <$> lookupKind dName
      let argKinds = map (\a -> fromJust $ Map.lookup a argTypeDict) dArgs
      unifyKinds dSpan k (foldr kindFun kindType argKinds)
      pure ()
  -- Collect for env
  s <- getKindSubst
  let
    kindEnv = map (tidyKind . applyKindSubst s) dTypeDict
    typeEnv = do
      ExDataDecl {..} <- dataDecls
      let result = foldl typeApp (typeConstructor dName) (map typeVar dArgs)
      (cname, cargs) <- dConstrs
      let ty = foldr (-->) result (stripAnn <$> cargs)
      pure $ (cname, ty)
    quant t = ForAll (Set.toList $ getFtvs t) [] t
    polyEnv = map (id *** quant) typeEnv
  pure (kindEnv, map defaultEnvType $ Map.fromList polyEnv)

checkValueDecls
  :: Map Text PolyType
  -> [ExValueDecl]
  -> Check [(Text, Intermed, PolyType)]
checkValueDecls declaredTypes decls = do
  let
    (impls, expls) = partitionEithers $ flip map decls $ \ExValueDecl {..} ->
      case Map.lookup vName declaredTypes of
        Nothing -> Left (vName, vExpr)
        Just p -> Right (vName, vExpr, p)
  (intermeds, ds) <- inferDefinitionGroup expls impls
  unless (null ds) $ internalError Nothing $
    "Deferred the following constraints while checking top-level definitions: "
    <> prettyConstraints ds
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
