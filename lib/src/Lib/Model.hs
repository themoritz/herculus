{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Model where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Bson              ((=:))
import qualified Data.Bson              as Bson
import           Data.Monoid
import           Data.Proxy
import           Data.Text              (Text)

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Dependencies
import           Lib.Model.Types
import           Lib.Types

class (ToDocument a, FromDocument a) => Model a where
  collectionName :: Proxy a -> Text

instance Model Project      where collectionName = const "projects"
instance Model Table        where collectionName = const "tables"
instance Model Column       where collectionName = const "columns"
instance Model Record       where collectionName = const "records"
instance Model Cell         where collectionName = const "cells"
instance Model Dependencies where collectionName = const "dependencies"

data Entity a = Entity
  { entityId  :: Id a
  , entityVal :: a
  } deriving (Generic)

instance ToJSON a => ToJSON (Entity a)
instance FromJSON a => FromJSON (Entity a)

instance ToDocument a => ToDocument (Entity a) where
  toDocument (Entity i x) =
    [ "_id" =: toObjectId i
    ] <> toDocument x

instance FromDocument a => FromDocument (Entity a) where
  parseDocument doc = Entity <$> (fromObjectId <$> Bson.lookup "_id" doc)
                             <*> parseDocument doc
