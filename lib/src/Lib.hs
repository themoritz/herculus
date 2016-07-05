{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Data.Aeson
import           Data.Map     (Map)
import           Data.Text

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

newtype Id a = Id Int
  deriving (Eq, Ord, Show, Generic)

instance ToJSON (Id a)
instance FromJSON (Id a)

newtype Ref a = Ref Text
  deriving (Show, Generic)

instance ToJSON (Ref a)
instance FromJSON (Ref a)

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
