{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Model.Types where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Text       (Text, pack)
import           Data.Typeable

import           Data.Bson       (Val, (=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Types


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
  { columnTableId :: Id Table
  , columnName :: Text
  , columnType :: ColumnType
  } deriving (Generic)

instance ToJSON Column
instance FromJSON Column

instance ToDocument Column where
  toDocument (Column t name typ) =
    [ "tableId" =: toObjectId t
    , "name" =: name
    , "type" =: toValue typ
    ]

instance FromDocument Column where
  parseDocument doc = do
    t <- Bson.lookup "tableId" doc
    name <- Bson.lookup "name" doc
    typVal <- Bson.lookup "type" doc
    case eitherDecodeValue typVal of
      Right typ -> pure $ Column (fromObjectId t)name typ
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
  { recordTableId :: Id Table
  }
  deriving (Generic)

instance ToJSON Record
instance FromJSON Record

instance ToDocument Record where
  toDocument (Record tblId)=
    [ "tableId" =: toObjectId tblId
    ]

instance FromDocument Record where
  parseDocument doc = Record <$> Bson.lookup "tableId" doc

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
