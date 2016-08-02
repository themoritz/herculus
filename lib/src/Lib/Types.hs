{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib.Types where

import           Data.Aeson      (FromJSON (..), ToJSON (..))
import           Data.Bson       (Document, ObjectId (..), Val, (=:))
import           Data.Decimal
import           Data.Monoid     ((<>))
import           Data.String
import           Data.Text       (Text, pack, unpack)
import           Data.Text.Encoding
import           Data.Serialize

import           Text.Read       (readMaybe)

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

instance Serialize (Id a) where
  put (Id (Oid x y)) = put x *> put y
  get = Id <$> (Oid <$> get <*> get)

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

class ParseValue a where
  parseValue :: Value -> Maybe a

class ShowValue a where
  showValue :: a -> Value

instance ParseValue Text where
  parseValue (Value s) = Just s

instance ShowValue Text where
  showValue = Value

instance ParseValue Number where
  parseValue (Value x) = Number <$> (readMaybe $ unpack x)

instance ShowValue Number where
  showValue (Number x) = Value . pack . show $ x

instance ShowValue a => ShowValue [a] where
  showValue as = "show list not implemented"

--

data CellResult
  = CellOk Value
  | CellParseError Text
  | CellEvalError Text

--

newtype Number = Number Decimal
  deriving (Num, Show)

instance ToJSON Number where
  toJSON (Number x) = toJSON . show $ x

instance FromJSON Number where
  parseJSON json = do
    x <- parseJSON json
    case readMaybe x of
      Nothing -> fail "could not read number"
      Just x' -> pure $ Number x'

instance Serialize Number where
  put (Number x) = put (BinaryDecimal x)
  get = Number . unBinaryDecimal <$> get

--

newtype BinaryDecimal = BinaryDecimal { unBinaryDecimal :: Decimal }

instance Serialize BinaryDecimal where
  put (BinaryDecimal (Decimal places mantissa)) = put places >> put mantissa
  get = BinaryDecimal <$> (Decimal <$> get <*> get)

--

newtype Utf8Text = Utf8Text { unUtf8Text :: Text }

instance Serialize Utf8Text where
    put = put . encodeUtf8 . unUtf8Text
    get = (Utf8Text . decodeUtf8) <$> get
