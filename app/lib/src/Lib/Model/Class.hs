{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Lib.Model.Class where

import           Lib.Prelude

import           Data.Bson

class (ToDocument a, FromDocument a) => Model a where
  collectionName :: Proxy a -> Text

class ToDocument a where
  toDocument :: a -> Document

class FromDocument a where
  parseDocument :: Document -> Either Text a
