{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib where

import           Data.Aeson       (FromJSON, ToJSON, genericParseJSON,
                                   genericToJSON, parseJSON, toJSON)
import qualified Data.Aeson.Types as Aeson (Options (..), defaultOptions)
import           Data.Bson        (Document, ObjectId (..), Val, (=:))
import qualified Data.Bson        as Bson
import           Data.Char        (toLower)
import           Data.Map         (Map)
import           Data.Monoid      ((<>))
import           Data.Proxy
import           Data.String
import           Data.Text        (Text, pack, unpack)
import           Data.Typeable
import           Text.Read        (readMaybe)

import           Data.Aeson.Bson
import           Lib.Base64
import           Lib.NamedMap

import           GHC.Generics

import           Web.HttpApiData

newtype Id a = Id ObjectId
  deriving (Eq, Ord, Val, Show, Read, Generic)

toObjectId :: Id a -> ObjectId
toObjectId (Id x) = x

fromObjectId :: ObjectId -> Id a
fromObjectId = Id

instance ToJSON (Id a) where
  -- Ideally use show instance of ObjectId, but Read
  -- instance does not work :/
  toJSON (Id (Oid a b)) = toJSON (a, b)

instance FromJSON (Id a) where
  parseJSON json = do
    (a, b) <- parseJSON json
    pure $ Id $ Oid a b

instance FromHttpApiData (Id a) where
  parseUrlPiece piece = case readMaybe $ unpack piece of
    Nothing -> Left $ "Expected 12 byte hex string, found " <> piece
    Just i  -> Right $ Id i

instance ToHttpApiData (Id a) where
  toUrlPiece (Id i) = pack $ show i

instance ToName (Id a) where
  toName (Id obj) = pack $ show obj

instance FromName (Id a) where
  fromName txt = Id $ read $ unpack txt

--

newtype Ref a = Ref Text
  deriving (Show, Generic, Eq, Val)

instance ToJSON (Ref a)
instance FromJSON (Ref a)

--

newtype Value = Value { unValue :: Text }
  deriving (Show, Eq, IsString, Monoid, Generic, Val)

instance ToJSON Value
instance FromJSON Value

-- Model

class (ToDocument a, FromDocument a) => Model a where
  collectionName :: Proxy a -> Text

instance Model Project where collectionName = const "projects"
instance Model Table   where collectionName = const "tables"
instance Model Column  where collectionName = const "columns"
instance Model Record  where collectionName = const "records"
instance Model Cell    where collectionName = const "cells"

data Entity a = Entity
  { entityId  :: Id a
  , entityVal :: a
  } deriving (Generic)

class ToDocument a where
  toDocument :: a -> Document

class FromDocument a where
  parseDocument :: Document -> Either Text a

instance ToJSON a => ToJSON (Entity a)
instance FromJSON a => FromJSON (Entity a)

instance ToDocument a => ToDocument (Entity a) where
  toDocument (Entity i x) =
    [ "_id" =: toObjectId i
    ] <> toDocument x

instance FromDocument a => FromDocument (Entity a) where
  parseDocument doc = Entity <$> (fromObjectId <$> Bson.lookup "_id" doc)
                             <*> parseDocument doc

stripOptions :: String -> Aeson.Options
stripOptions name = Aeson.defaultOptions { Aeson.fieldLabelModifier = modifier }
  where modifier = lowerHead . drop (length name)
        lowerHead [] = []
        lowerHead (h:tl) = toLower h : tl

data Project = Project
  { projectName :: Text
  } deriving (Generic)

instance ToJSON Project
instance FromJSON Project

instance ToDocument Project where
  toDocument (Project name) =
    [ "name" =: name
    ]

instance FromDocument Project where
  parseDocument doc = Project <$> Bson.lookup "name" doc

data Table = Table
  { tableProjectId :: Id Project
  , tableName      :: Text
  } deriving (Generic)

instance ToJSON Table
instance FromJSON Table

instance ToDocument Table where
  toDocument (Table prj name) =
    [ "projectId" =: toObjectId prj
    , "name" =: name
    ]

instance FromDocument Table where
  parseDocument doc = Table <$> (fromObjectId <$> Bson.lookup "projectId" doc)
                            <*> Bson.lookup "name" doc

data Column = Column
  { columnName :: Text
  , columnType :: ColumnType
  } deriving (Generic)

instance ToJSON Column
instance FromJSON Column

instance ToDocument Column where
  toDocument (Column name typ) =
    [ "name" =: name
    , "type" =: toValue typ
    ]

instance FromDocument Column where
  parseDocument doc = do
    name <- Bson.lookup "name" doc
    typVal <- Bson.lookup "type" doc
    case eitherDecodeValue typVal of
      Right typ -> pure $ Column name typ
      Left msg -> Left $ pack msg

data ColumnType
  = ColumnInput DataType
  | ColumnDerived Text
  deriving (Show, Generic)

instance ToJSON ColumnType
instance FromJSON ColumnType

instance ToBSON ColumnType
instance FromBSON ColumnType

data DataType
  = DataBoolean
  | DataString
  | DataNumber
  | DataRecord
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON DataType
instance FromJSON DataType

data Record = Record
  deriving (Generic)

instance ToJSON Record
instance FromJSON Record

instance ToDocument Record where
  toDocument = const []

instance FromDocument Record where
  parseDocument = const $ pure Record

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
  { cellValue   :: Value
  , cellAspects :: Aspects
  } deriving (Generic)

instance ToJSON Cell
instance FromJSON Cell

instance ToDocument Cell where
  toDocument (Cell val asp) =
    [ "value" =: val
    , "aspects" =: asp
    ]

instance FromDocument Cell where
  parseDocument doc = Cell <$> Bson.lookup "value" doc
                           <*> Bson.lookup "aspects" doc
