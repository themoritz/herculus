{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Record where

import Control.DeepSeq

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)

import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Table
import           Lib.Types

data Record = Record
  { recordTableId :: Id Table
  } deriving (Generic, NFData, Eq, Ord, Show)

instance Model Record where
  collectionName = const "records"

instance ToJSON Record
instance FromJSON Record

instance ToDocument Record where
  toDocument (Record tblId)=
    [ "tableId" =: toObjectId tblId
    ]

instance FromDocument Record where
  parseDocument doc = Record <$> Bson.lookup "tableId" doc
