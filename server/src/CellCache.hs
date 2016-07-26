{-# LANGUAGE TemplateHaskell #-}

module CellCache
  ( CellCache
  , emptyCellCache
  , store
  , retrieve
  , retrieveAll
  ) where

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Lib

newtype CellCache = CellCache
  { _unCellCache :: Map (Id Column, Id Record) Value
  }

makeLenses ''CellCache

emptyCellCache :: CellCache
emptyCellCache = CellCache Map.empty

store :: Id Column -> Id Record -> Value -> CellCache -> CellCache
store c r v = set (unCellCache . at (c, r)) (Just v)

retrieve :: Id Column -> Id Record -> CellCache -> Maybe Value
retrieve c r = view (unCellCache . at (c, r))

retrieveAll :: CellCache -> [(Id Column, Id Record, Value)]
retrieveAll = map (\((c, r), v) -> (c, r, v)) . Map.toList . _unCellCache
