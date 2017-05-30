module Herculus.Config.Column.Types where

import Herculus.Prelude
import Data.Map as Map
import Data.Array (mapMaybe, snoc)
import Data.Map (Map)
import Lib.Api.Schema.Compiler (Kind(..))

eqKind :: Kind -> Kind -> Boolean
eqKind KindType KindType = true
eqKind KindTable KindTable = true
eqKind (KindFun a b) (KindFun a' b') = eqKind a a' && eqKind b b'
eqKind _ _ = false

filterTypes :: Kind -> Map String Kind -> Array (Tuple String (Array Kind))
filterTypes goal = Map.toUnfoldable >>> mapMaybe go
  where
  go (Tuple n k) = case subTypes goal k of
    Nothing -> Nothing
    Just args -> Just (Tuple n args)

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

