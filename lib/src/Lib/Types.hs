{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Lib.Types where

import           Data.Aeson         (FromJSON (..), ToJSON (..))
import           Data.Bson          (ObjectId (..), Val (..))
import           Data.Decimal
import           Data.Monoid        ((<>))
import           Data.Serialize
import           Data.Text          (Text, pack, unpack)
import           Data.Text.Encoding

import           Text.Read          (readMaybe)

import           GHC.Generics

import           Web.HttpApiData

import           Lib.NamedMap

newtype Id a = Id ObjectId
  deriving (Eq, Ord, Val, Show, Read, Generic)

toObjectId :: Id a -> ObjectId
toObjectId (Id x) = x

fromObjectId :: ObjectId -> Id a
fromObjectId = Id

nullObjectId :: Id a
nullObjectId = Id (Oid 0 0)

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

instance Serialize (Id a) where
  put (Id (Oid x y)) = put x *> put y
  get = Id <$> (Oid <$> get <*> get)

--

newtype Ref a = Ref Text
  deriving (Show, Generic, Eq, Ord, Val)

instance ToJSON (Ref a)
instance FromJSON (Ref a)

--

newtype Number = Number Decimal
  deriving (Num, Eq, Ord)

instance Show Number where
  show (Number x) = show x

instance ToJSON Number where
  toJSON (Number x) = toJSON . show $ x

instance FromJSON Number where
  parseJSON json = do
    x <- parseJSON json
    case readMaybe x of
      Nothing -> fail "could not read number"
      Just x' -> pure $ Number x'

instance Serialize Number where
  put (Number (Decimal places mantissa)) = put places >> put mantissa
  get = Number <$> (Decimal <$> get <*> get)

--

newtype Utf8Text = Utf8Text { unUtf8Text :: Text }

instance Serialize Utf8Text where
    put = put . encodeUtf8 . unUtf8Text
    get = (Utf8Text . decodeUtf8) <$> get
