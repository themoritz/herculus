{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Model.ViewState where

import           Lib.Prelude

import           Control.Lens      (makeLenses)

import           Data.Bson         ((=:))
import qualified Data.Bson         as Bson

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types         (Id, fromObjectId, toObjectId)

--------------------------------------------------------------------------------

data ColumnWidth = ColumnWidth
  { _cwProjectId :: Id Project
  , _cwColumnId  :: Id Column
  , _cwWidth     :: Int
  }

makeLenses ''ColumnWidth

instance Model ColumnWidth where
  collectionName = const "columnWidths"

instance ToDocument ColumnWidth where
  toDocument (ColumnWidth projectId columnId width) =
    [ "projectId" =: toObjectId projectId
    , "columnId" =: toObjectId columnId
    , "width" =: width
    ]

instance FromDocument ColumnWidth where
  parseDocument doc =
    ColumnWidth <$> (fromObjectId <$> Bson.lookup "projectId" doc)
                <*> (fromObjectId <$> Bson.lookup "columnId" doc)
                <*> Bson.lookup "width" doc

--------------------------------------------------------------------------------

data ColumnOrder = ColumnOrder
  { _coTableId :: Id Table
  , _coOrder   :: [Id Column]
  }

makeLenses ''ColumnOrder

instance Model ColumnOrder where
  collectionName = const "columnOrders"

instance ToDocument ColumnOrder where
  toDocument (ColumnOrder tableId order) =
    [ "tableId" =: toObjectId tableId
    , "order" =: map toObjectId order
    ]

instance FromDocument ColumnOrder where
  parseDocument doc =
    ColumnOrder <$> (fromObjectId <$> Bson.lookup "tableId" doc)
                <*> (map fromObjectId <$> Bson.lookup "order" doc)
