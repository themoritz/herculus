module Herculus.Utils.Ordering where

import Herculus.Prelude
import Data.Map as Map
import Data.Array (deleteAt, insertAt, mapMaybe, (!!))
import Data.Map (Map)

data Relative
  = Before
  | After

data RelativeTo a = RelativeTo Relative a

derive instance functorRelativeTo :: Functor RelativeTo

getRelativeTarget :: forall a. RelativeTo a -> a
getRelativeTarget (RelativeTo _ ix) = ix

reorder :: forall a. Index -> RelativeTo Index -> Array a -> Array a
reorder ix (RelativeTo rel target) arr = fromMaybe arr do
  a <- arr !! ix
  case rel of
    Before ->
      case compare ix target of
        LT -> insertAt target a arr >>= deleteAt ix
        GT -> deleteAt ix arr >>= insertAt target a
        EQ -> pure arr
    After -> do
      case compare ix target of
        LT -> insertAt (target + 1) a arr >>= deleteAt ix
        GT -> deleteAt ix arr >>= insertAt (target + 1) a
        EQ -> pure arr
    
orderMap
  :: forall a b. Ord a => Array a -> Map a b -> Array (Tuple a b)
orderMap order m = ordered <> rest
  where
    ordered = mapMaybe (\a -> Tuple a <$> Map.lookup a m) order
    rest = Map.toAscUnfoldable $ foldr Map.delete m order
