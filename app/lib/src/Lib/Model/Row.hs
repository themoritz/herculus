{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Model.Row where

import           Control.Lens

import           Data.Aeson      (FromJSON, ToJSON)

import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Table
import           Lib.Types

data Row = Row
  { _rowTableId :: Id Table
  } deriving (Generic, Eq, Ord, Show)

rowTableId :: Lens' Row (Id Table)
rowTableId = lens _rowTableId (\s a -> s { _rowTableId = a })

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
