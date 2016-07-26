{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib where

import           Data.Aeson      (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Bson       (Document, ObjectId (..), Val, (=:))
import           Data.Map        (Map)
import           Data.Monoid     ((<>))
import           Data.String
import           Data.Text
import           Text.Read       (readMaybe)

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

data Project = Project
  { projectId   :: Id Project
  , projectName :: Text
  } deriving (Generic)

instance ToJSON Project
instance FromJSON Project

data Table = Table
  { tableId        :: Id Table
  , tableProjectId :: Id Project
  , tableName      :: Text
  } deriving (Generic)

instance ToJSON Table
instance FromJSON Table

data Column = Column
  { columnId   :: Id Column
  , columnName :: Text
  , columnType :: ColumnType
  } deriving (Generic)

instance ToJSON Column
instance FromJSON Column

data ColumnType
  = ColumnInput DataType
  | ColumnDerived Text
  deriving (Show, Generic)

instance ToJSON ColumnType
instance FromJSON ColumnType

instance ToValue ColumnType
instance FromValue ColumnType

data DataType
  = DataBoolean
  | DataString
  | DataNumber
  | DataRecord
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON DataType
instance FromJSON DataType

data Record = Record
  { recordId :: Id Record
  } deriving (Generic)

instance ToJSON Record
instance FromJSON Record

data Cell = Cell
  { cellId    :: Id Cell
  , cellValue :: Value
  }
