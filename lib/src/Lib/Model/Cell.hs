{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Model.Cell where

import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Aeson.Bson  (FromBSON, ToBSON)
import           Data.Bson        (Val, (=:))
import qualified Data.Bson        as Bson
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text, unpack)
import           Data.Typeable

import           Text.Read        (readMaybe)

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

--

data Value
  = VString Text
  | VNumber Number
  | VBool Bool
  | VRecord (Id Record)
  | VList [Value]
  | VMaybe (Maybe Value)
  deriving (Generic, Typeable, Show, Eq)

instance ToJSON Value
instance FromJSON Value

class ParseValue a where
  parseValue :: Text -> Maybe a

instance ParseValue Text where
  parseValue s = Just s

instance ParseValue Number where
  parseValue s = Number <$> (readMaybe $ unpack s)

class ExtractValue a where
  extractValue :: Value -> Maybe a

instance ExtractValue Text where
  extractValue (VString s) = Just s
  extractValue _ = Nothing

instance ExtractValue Number where
  extractValue (VNumber n) = Just n
  extractValue _ = Nothing

class MakeValue a where
  makeValue :: a -> Maybe Value

instance MakeValue Text where
  makeValue = Just . VString

instance MakeValue Number where
  makeValue = Just . VNumber

instance MakeValue [a] where
  makeValue = const Nothing

extractValue' :: ExtractValue a => Value -> a
extractValue' = fromMaybe (error "expexted certain value") . extractValue

--

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
