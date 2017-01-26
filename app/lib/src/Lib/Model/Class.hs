{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Lib.Model.Class where

import           Data.Bson
import           Data.Proxy
import           Data.Text

import           Lib.Types

class (ToDocument a, FromDocument a) => Model a where
  collectionName :: Proxy a -> Text

class ToDocument a where
  toDocument :: a -> Document

class FromDocument a where
  parseDocument :: Document -> Either Text a

class ClientModel server client | client -> server where
  toClient     :: server -> client
  toClientId   :: Id server -> Id client
  toClientId (Id i) = Id i
  fromClient   :: client -> server
  fromClientId :: Id client -> Id server
  fromClientId (Id i) = Id i
