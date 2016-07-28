{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib.Types where

import Data.Aeson hiding (Value)
import           Data.Bson        (Document, ObjectId (..), Val, (=:))
import           Data.Text        (Text, pack, unpack)
import           Data.Monoid      ((<>))
import           Data.String

import           Text.Read        (readMaybe)

import           GHC.Generics

import           Web.HttpApiData

import           Lib.NamedMap

newtype Id a = Id ObjectId
  deriving (Eq, Ord, Val, Show, Read, Generic)

toObjectId :: Id a -> ObjectId
toObjectId (Id x) = x

fromObjectId :: ObjectId -> Id a
fromObjectId = Id

instance ToJSON (Id a) where
  -- Ideally use show instance of ObjectId, but Read
  -- instance does not work :/
  toJSON (Id (Oid a b)) = toJSON (a, b)

instance FromJSON (Id a) where
  parseJSON json = do
    (a, b) <- parseJSON json
    pure $ Id $ Oid a b

instance FromHttpApiData (Id a) where
  parseUrlPiece piece = case readMaybe $ unpack piece of
    Nothing -> Left $ "Expected 12 byte hex string, found " <> piece
    Just i  -> Right $ Id i

instance ToHttpApiData (Id a) where
  toUrlPiece (Id i) = pack $ show i

instance ToName (Id a) where
  toName (Id obj) = pack $ show obj

instance FromName (Id a) where
  fromName txt = Id $ read $ unpack txt

--

newtype Ref a = Ref Text
  deriving (Show, Generic, Eq, Val)

instance ToJSON (Ref a)
instance FromJSON (Ref a)

--

newtype Value = Value { unValue :: Text }
  deriving (Show, Eq, IsString, Monoid, Generic, Val)

instance ToJSON Value
instance FromJSON Value
