{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

module Lib.Model.Class where

import           Control.Monad.Fail(MonadFail(..))
import qualified Data.Text as T

import           Lib.Prelude

import           Data.Bson

class (ToDocument a, FromDocument a) => Model a where
  collectionName :: Proxy a -> Text

class ToDocument a where
  toDocument :: a -> Document

class FromDocument a where
  parseDocument :: Document -> Either Text a

-- | This instance is needed by Bson.lookup
instance MonadFail (Either Text) where
    fail msg = Left (T.pack msg)
