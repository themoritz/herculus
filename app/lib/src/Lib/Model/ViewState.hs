{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Model.ViewState where

import           Control.Lens      (makeLenses)

import           Data.Bson         ((=:))
import qualified Data.Bson         as Bson
import           Data.Serialize    (decode, encode)
import           Data.Text         (Text)

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Project
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
  { _coProjectId :: Id Project
  , _coColumnId  :: Id Column
  , _coPrevious  :: Maybe (Id Column)
  }

makeLenses ''ColumnOrder

instance Model ColumnOrder where
  collectionName = const "columnOrder"

instance ToDocument ColumnOrder where
  toDocument (ColumnOrder projectId columnId previous) =
    [ "projectId" =: toObjectId projectId
    , "columnId" =: toObjectId columnId
    , "previous" =: toObjectId <$> previous
    ]

instance FromDocument ColumnOrder where
  parseDocument doc =
    ColumnOrder <$> (fromObjectId <$> Bson.lookup "projectId" doc)
                <*> (fromObjectId <$> Bson.lookup "columnId" doc)
                <*> (fmap fromObjectId <$> Bson.lookup "previous" doc)
