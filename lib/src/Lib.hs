{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Aeson   (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Bson    (Document, UUID (..), Value (..), (=:))
import           Data.Map     (Map)
import           Data.Text

import           Lib.Base64
import           Data.Aeson.Bson

import           GHC.Generics

-- WebSocket protocol

data WsUpMessage
  -- Play
  = WsUpGreet Text
  | WsUpStore Text
  | WsUpList
  -- Modify
  | WsUpCreateProject Text
  | WsUpCreateTable (Id Project) Text
  | WsUpCreateColumn (Id Table) Text ColumnType
  | WsUpAddRow (Id Table) [(Id Column, Text)]
  -- Retrieve
  | WsUpListTables (Id Project)
  | WsUpListProjects
  deriving (Generic, Show)

instance ToJSON WsUpMessage
instance FromJSON WsUpMessage

data WsDownMessage
  -- Play
  = WsDownGreet Text
  | WsDownList [Text]
  -- Provide
  | WsDownTables (Id Project) [(Id Table, Text)]
  | WsDownProjects [(Id Project, Text)]
  deriving (Generic, Show)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage

--

newtype Id a = Id UUID
  deriving (Eq, Ord, Show, Generic)

uuId :: Id a -> UUID
uuId (Id x) = x

instance ToJSON (Id a) where
  toJSON (Id (UUID uuid)) = toJSON $ Base64 uuid

instance FromJSON (Id a) where
  parseJSON json = do
    (Base64 base64) <- parseJSON json
    pure $ Id $ UUID base64

--

newtype Ref a = Ref Text
  deriving (Show, Generic)

instance ToJSON (Ref a)
instance FromJSON (Ref a)

--

class ToJSON a => ToDocument a where
  toDocument :: a -> Document
  toDocument = toBson . toJSON

class FromJSON a => FromDocument a where
  fromDocument :: Document -> Either String a
  fromDocument = parseJSON . toJSON

--

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
