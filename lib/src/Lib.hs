{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Data.Aeson   (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Bson    (Val, Document, ObjectId (..), (=:))
import           Data.Map     (Map)
import           Data.Text
import Data.String
import Text.Read (readMaybe)

import           Lib.Base64
import           Data.Aeson.Bson

import           GHC.Generics

import           Web.HttpApiData

newtype Id a = Id ObjectId
  deriving (Eq, Ord, Show, Read, Generic)

toObjectId :: Id a -> ObjectId
toObjectId (Id x) = x

fromObjectId :: ObjectId -> Id a
fromObjectId = Id

instance ToJSON (Id a) where
  toJSON (Id objId) = toJSON $ show objId

instance FromJSON (Id a) where
  parseJSON json = do
    str <- parseJSON json
    maybe (fail "expected 12 byte hex string") pure $ readMaybe str

instance FromHttpApiData (Id a) where
  parseUrlPiece = maybe (Left "expected 12 byte hex string") Right . readMaybe . unpack

instance ToHttpApiData (Id a) where
  toUrlPiece = pack . show

--

newtype Ref a = Ref Text
  deriving (Show, Generic)

instance ToJSON (Ref a)
instance FromJSON (Ref a)

--

newtype Value = Value Text
  deriving (Show, Eq, IsString, Generic, Val)

instance ToJSON Value
instance FromJSON Value

data Project = Project
  { projectName   :: Text
  , projectTables :: [Table]
  }

data Table = Table
  { tableName    :: Text
  , tableColumns :: [Column]
  }

data Column = Column
  { columnName :: Text
  , columnType :: ColumnType
  }

data ColumnType
  = ColumnInput DataType
  | ColumnDerived Expression
  deriving (Show, Generic)

instance ToJSON ColumnType
instance FromJSON ColumnType

instance ToValue ColumnType
instance FromValue ColumnType

data DataType
  = DataBoolean
  | DataString
  | DataNumber
  | DataList DataType
  | DataRecord
  deriving (Show, Generic)

instance ToJSON DataType
instance FromJSON DataType

data Expression
  = ExprColumnRef (Ref Table) (Ref Column)
  deriving (Show, Generic)

instance ToJSON Expression
instance FromJSON Expression

data Record = Record
  { recordData :: Map (Id Column) Text
  }

data Cell
