{-# LANGUAGE UndecidableInstances #-}

module Lib.NamedMap where

import           Control.Lens

import           Data.Aeson
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Text

class ToName a where
  toName :: a -> Text

class FromName a where
  fromName :: Text -> a

newtype NamedMap k v = NamedMap { unNamedMap :: Map k v }
  deriving (Eq, Ord)

namedMap :: Lens' (NamedMap k v) (Map k v)
namedMap = lens unNamedMap (const NamedMap)

emptyNamedMap :: NamedMap k v
emptyNamedMap = NamedMap Map.empty

instance (ToName k, ToJSON v) => ToJSON (NamedMap k v) where
  toJSON = toJSON . Map.mapKeys toName . unNamedMap

instance (Ord k, FromName k, FromJSON v) => FromJSON (NamedMap k v) where
  parseJSON json = do
    m <- parseJSON json
    pure . NamedMap . Map.mapKeys fromName $ m
