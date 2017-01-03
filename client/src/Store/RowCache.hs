{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Store.RowCache
  ( module Action.RowCache
  , runAction
  , recordCache
  , empty
  , State
  ) where

import           Control.Arrow    (second)
import           Control.Lens
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Lib.Model        (Entity (Entity))
import           Lib.Model.Cell   (CellContent)
import           Lib.Model.Column (Column)
import           Lib.Model.Row    (Row)
import           Lib.Model.Table  (Table)
import           Lib.Types        (Id)

import           Action.RowCache  (Action (..))

data State = State
  { _recordCache :: Map (Id Row) (Map (Id Column) (Column, CellContent))
  }

empty :: State
empty = State
  { _recordCache = Map.empty
  }

makeLenses ''State

runAction :: Id Table
          -> Action.RowCache.Action
          -> State -> State
runAction tableId = \case
    Add recordId record ->
      let toCol (Entity c col, content) = (c, (col, content))
          recMap = Map.fromList $ map toCol record
      in  recordCache . at recordId .~ Just recMap

    -- record cache get is in src/Action

    Delete r -> recordCache . at r .~ Nothing

    Set recs ->
      let toCol (Entity c col, content) = (c, (col, content))
          recMaps = map (second $ Map.fromList . map toCol) recs
      in  recordCache .~ Map.fromList recMaps
