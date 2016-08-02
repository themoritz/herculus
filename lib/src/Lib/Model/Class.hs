module Lib.Model.Class where

import Data.Bson
import Data.Text
import           Data.Proxy

class (ToDocument a, FromDocument a) => Model a where
  collectionName :: Proxy a -> Text

class ToDocument a where
  toDocument :: a -> Document

class FromDocument a where
  parseDocument :: Document -> Either Text a
