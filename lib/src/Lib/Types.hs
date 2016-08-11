{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Lib.Types where

import           Data.Aeson         (FromJSON (..), ToJSON (..), object, (.:),
                                     (.=))
import qualified Data.Aeson         as Aeson
import           Data.Aeson.Bson
import           Data.Aeson.Types   (Parser)
import           Data.Bson          (Document, ObjectId (..), Val (..), (=:))
import           Data.Decimal
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))
import           Data.Serialize
import           Data.String
import           Data.Text          (Text, pack, unpack)
import           Data.Text.Encoding
import           Data.Typeable

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

data DataType
  = DataBool
  | DataString
  | DataNumber
  | DataRecord
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON DataType
instance FromJSON DataType

instance ToBSON DataType
instance FromBSON DataType

data TType a where
  TypeString :: TType Text
  TypeNumber :: TType Number
  TypeStringList :: TType [Text]
  TypeNumberList :: TType [Number]

deriving instance Show (TType a)

data Equal a b where
    Eq :: Equal a a

checkEqual :: TType a -> TType b -> Maybe (Equal a b)
checkEqual TypeNumber    TypeNumber    = Just Eq
checkEqual TypeString    TypeString    = Just Eq
checkEqual _             _             = Nothing

data SigOk a where
  Ok :: SigOk a

checkSig :: DataType -> TType a -> Maybe (SigOk a)
checkSig DataString TypeString = Just Ok
checkSig DataNumber TypeNumber = Just Ok
checkSig _          _          = Nothing

--

data Value
  = ValueString Text
  | ValueNumber Number
  deriving (Generic, Typeable, Show, Eq)

instance ToJSON Value
instance FromJSON Value

class ParseValue a where
  parseValue :: Text -> Maybe a

instance ParseValue Text where
  parseValue s = Just s

instance ParseValue Number where
  parseValue s = Number <$> (readMaybe $ unpack s)

class ExtractValue a where
  extractValue :: Value -> Maybe a

instance ExtractValue Text where
  extractValue (ValueString s) = Just s
  extractValue _ = Nothing

instance ExtractValue Number where
  extractValue (ValueNumber n) = Just n
  extractValue _ = Nothing

class MakeValue a where
  makeValue :: a -> Maybe Value

instance MakeValue Text where
  makeValue = Just . ValueString

instance MakeValue Number where
  makeValue = Just . ValueNumber

instance MakeValue [a] where
  makeValue = const Nothing

extractValue' :: ExtractValue a => Value -> a
extractValue' = fromMaybe (error "expexted certain value") . extractValue

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
