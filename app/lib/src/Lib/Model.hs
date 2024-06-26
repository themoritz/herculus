{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib.Model where

import           Lib.Prelude

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson

import           Lib.Model.Class
import           Lib.Types

data Entity a = Entity
  { entityId  :: Id a
  , entityVal :: a
  } deriving (Typeable, Generic, Show)

entityToTuple :: Entity a -> (Id a, a)
entityToTuple (Entity i a) = (i, a)

tupleToEntity :: (Id a, a) -> Entity a
tupleToEntity (i, a) = Entity i a

instance ToJSON a => ToJSON (Entity a)
instance FromJSON a => FromJSON (Entity a)

instance ToDocument a => ToDocument (Entity a) where
  toDocument (Entity i x) =
    [ "_id" =: toObjectId i
    ] <> toDocument x

instance FromDocument a => FromDocument (Entity a) where
  parseDocument doc = Entity <$> (fromObjectId <$> Bson.lookup "_id" doc)
                             <*> parseDocument doc
