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

import           GHC.Generics

import           Web.HttpApiData

newtype Id a = Id ObjectId
  deriving (Eq, Ord, Show, Read, Generic)

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
  toUrlPiece (Id i)= pack $ show i

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
