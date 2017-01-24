module Database.MongoDb.Class where

import qualified Data.Bson as Bson

class FromDocument x where
  fromDocument :: Bson.Document -> Maybe x

class ToDocument x where
  toDocument :: x -> Bson.Document
