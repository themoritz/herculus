-- |

module Lib.Utils where

import           Lib.Prelude

import qualified Data.IntMap   as IntMap
import qualified Data.Map      as Map

import           Data.Hashable (Hashable, hash)

hashMapKeys :: Hashable k => Map k v -> IntMap v
hashMapKeys = IntMap.fromAscList . Map.toAscList . Map.mapKeys hash
