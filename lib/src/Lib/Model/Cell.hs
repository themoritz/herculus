{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Model.Cell where

import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Aeson.Bson  (FromBSON, ToBSON)
import           Data.Bson        (Val, (=:))
import qualified Data.Bson        as Bson
import           Data.Text        (Text)
import           Data.Typeable

import           GHC.Generics

import           Lib.Model
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

data CellContent
  = CellNothing
  | CellValue Value
  | CellEvalError Text
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON CellContent
instance FromJSON CellContent

instance ToBSON CellContent
instance FromBSON CellContent

instance Val CellContent where
  val = toValue
  cast' = decodeValue

data Aspects = Aspects
  { aspectsTableId  :: Id Table
  , aspectsColumnId :: Id Column
  , aspectsRecordId :: Id Record
  } deriving (Generic, Eq, Show, Typeable)

instance ToJSON Aspects
instance FromJSON Aspects

instance Val Aspects where
  val (Aspects t c r) = Bson.Doc
    [ "tableId" =: toObjectId t
    , "columnId" =: toObjectId c
    , "recordId" =: toObjectId r
    ]
  cast' (Bson.Doc doc) = do
    t <- Bson.lookup "tableId" doc
    c <- Bson.lookup "columnId" doc
    r <- Bson.lookup "recordId" doc
    pure $ Aspects t c r
  cast' _ = fail "expected document"

data Cell = Cell
  { cellContent :: CellContent
  , cellAspects :: Aspects
  } deriving (Generic)

emptyCell :: Id Table -> Id Column -> Id Record -> Cell
emptyCell t c r = Cell
  { cellContent = CellNothing
  , cellAspects = Aspects t c r
  }

instance Model Cell         where collectionName = const "cells"

instance ToJSON Cell
instance FromJSON Cell

instance ToDocument Cell where
  toDocument (Cell con asp) =
    [ "content" =: con
    , "aspects" =: asp
    ]

instance FromDocument Cell where
  parseDocument doc = Cell <$> Bson.lookup "content" doc
                           <*> Bson.lookup "aspects" doc