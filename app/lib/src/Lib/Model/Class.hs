{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Lib.Model.Class where

import           Data.Bson
import           Data.Proxy
import           Data.Text

class (ToDocument a, FromDocument a) => Model a where
  collectionName :: Proxy a -> Text

class ToDocument a where
  toDocument :: a -> Document

class FromDocument a where
  parseDocument :: Document -> Either Text a
