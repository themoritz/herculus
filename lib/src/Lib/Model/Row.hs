{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Model.Row where

import           Control.DeepSeq

import           Data.Aeson      (FromJSON, ToJSON)

import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Table
import           Lib.Types

data Row = Row
  { rowTableId :: Id Table
  } deriving (Generic, NFData, Eq, Ord, Show)

instance Model Row where
  collectionName = const "rows"

instance ToJSON Row
instance FromJSON Row

instance ToDocument Row where
  toDocument (Row tblId)=
    [ "tableId" =: toObjectId tblId
    ]

instance FromDocument Row where
  parseDocument doc = Row . fromObjectId <$> Bson.lookup "tableId" doc
