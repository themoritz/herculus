module Herculus.Config.Column.Types where

import Herculus.Prelude
import Data.Map as Map
import Data.Array (all, mapMaybe, snoc)
import Data.Map (Map)
import Lib.Api.Schema.Compiler (Kind(KindFun, KindRecord, KindTable, KindType), TyconInfo, Type(TypeRecord, TypeTable, TypeApp, TypeConstructor, TypeVar), tyconKind, tyconValueConstrs)

eqKind :: Kind -> Kind -> Boolean
eqKind KindType KindType = true
eqKind KindTable KindTable = true
eqKind (KindFun a b) (KindFun a' b') = eqKind a a' && eqKind b b'
eqKind _ _ = false

filterTypes :: Kind -> Map String TyconInfo -> Array (Tuple String (Array Kind))
filterTypes goal = Map.toUnfoldable >>> mapMaybe go
  where
  go (Tuple n info) = if noFunction n info
    then case subTypes goal (info ^. tyconKind) of
      Nothing -> Nothing
      Just args -> Just (Tuple n args)
    else Nothing
  noFunction :: String -> TyconInfo -> Boolean
  noFunction name info = name /= "->" && validConstrs
    where
    validConstrs = all validConstr (info ^. tyconValueConstrs)
    validConstr (Tuple _ ts) = all validType ts
    validType = case _ of
      TypeVar _ -> true
      TypeConstructor c -> c /= "->"
      TypeApp f arg -> validType f && validType arg
      TypeTable _ -> true
      TypeRecord _ -> true

subTypes :: Kind -> Kind -> Maybe (Array Kind)
subTypes goal = go []
  where
  go :: Array Kind -> Kind -> Maybe (Array Kind)
  go args k = case eqKind k goal of
    true -> Just args
    false -> case k of
      KindType -> Nothing
      KindTable -> Nothing
      KindRecord -> Nothing
      KindFun arg res -> go (args `snoc` arg) res
