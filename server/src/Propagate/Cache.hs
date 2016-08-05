{-# LANGUAGE TemplateHaskell #-}

module Propagate.Cache
  ( Cache
  , empty
  , storeCell
  , getCell
  , getAllCells
  , storeColumn
  , getColumn
  , storeCode
  , getCode
  ) where

import           Control.Lens

import           Data.Map         (Map)
import qualified Data.Map         as Map

import           Lib.Model.Column
import           Lib.Model.Cell
import           Lib.Model.Types
import           Lib.Types

data Cache = Cache
  { _cacheCell          :: Map (Id Column, Id Record) CellContent
  , _cacheColumn        :: Map (Id Column) [CellContent]
  , _cacheCompileResult :: Map (Id Column) CompileResult
  }

makeLenses ''Cache

empty :: Cache
empty = Cache Map.empty Map.empty Map.empty

--

storeCell :: Id Column -> Id Record -> CellContent -> Cache -> Cache
storeCell c r v = set (cacheCell . at (c, r)) (Just v)

getCell :: Id Column -> Id Record -> Cache -> Maybe CellContent
getCell c r = view (cacheCell . at (c, r))

getAllCells :: Cache -> [(Id Column, Id Record, CellContent)]
getAllCells = map (\((c, r), v) -> (c, r, v)) . Map.toList . _cacheCell

--

storeColumn :: Id Column -> [CellContent] -> Cache -> Cache
storeColumn c vs = set (cacheColumn . at c) (Just vs)

getColumn :: Id Column -> Cache -> Maybe [CellContent]
getColumn c = view (cacheColumn . at c)

--

storeCode :: Id Column -> CompileResult -> Cache -> Cache
storeCode c expr = set (cacheCompileResult . at c) (Just expr)

getCode :: Id Column -> Cache -> Maybe CompileResult
getCode c = view (cacheCompileResult . at c)
