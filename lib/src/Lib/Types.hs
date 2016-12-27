{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Lib.Types where

import           Control.DeepSeq
import           Control.Exception  (SomeException, displayException, evaluate,
                                     try)

import           Data.Aeson         (FromJSON (..), ToJSON (..))
import           Data.Bson          (ObjectId (..), Val (..))
import           Data.Decimal
import           Data.Monoid        ((<>))
import           Data.Serialize
import           Data.Text          (Text, pack, unpack)
import           Data.Text.Encoding
import           Data.Time.Calendar (Day (..))
import           Data.Time.Clock    (NominalDiffTime, UTCTime (..), addUTCTime)
import           Data.Time.Format   (defaultTimeLocale, parseTimeM)
import qualified Data.Time.Format   as T (formatTime)
import           Data.Typeable      (Typeable)

import           Text.Printf        (PrintfArg (..), formatRealFloat, printf)
import           Text.Read          (readMaybe)

import           GHC.Generics
import           System.IO.Unsafe   (unsafePerformIO)

import           Web.HttpApiData

import           Lib.NamedMap

newtype Id a = Id ObjectId
  deriving (Eq, Ord, Generic, Typeable)

instance Show (Id a) where
  show (Id objectId) = show objectId

instance Read (Id a) where
  readsPrec d r = do
    (v, r') <- readsPrec d r
    pure $ (Id v, r')

toObjectId :: Id a -> ObjectId
toObjectId (Id x) = x

fromObjectId :: ObjectId -> Id a
fromObjectId = Id

nullObjectId :: Id a
nullObjectId = Id (Oid 0 0)

instance NFData (Id a) where
  rnf (Id (Oid a b)) = rnf a `seq` rnf b

instance ToJSON (Id a) where
  toJSON (Id i) = toJSON $ show i

instance FromJSON (Id a) where
  parseJSON json = do
    str <- parseJSON json
    case readMaybe str of
      Nothing -> fail "could not read"
      Just i  -> pure (Id i)

instance FromHttpApiData (Id a) where
  parseUrlPiece piece = case readMaybe $ unpack piece of
    Nothing -> Left $ "Expected 12 byte hex string, found " <> piece
    Just i  -> Right (Id i)

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

newtype Ref a = Ref { unRef :: Text }
  deriving (Generic, Eq, Ord, Val, NFData)

instance Show (Ref a) where
  show = unpack . unRef

instance ToJSON (Ref a)
instance FromJSON (Ref a)

--

newtype Number = Number Decimal
  deriving (Num, Eq, Ord, NFData)

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

instance PrintfArg Number where
  formatArg (Number d) = formatRealFloat (read $ show d :: Double)

formatNumber :: Text -> Number -> Text
formatNumber f n = case unsafePerformIO (try $ evaluate $ printf (unpack f) n) of
  Left (e :: SomeException) -> pack $ displayException e
  Right str                 -> pack str

--

newtype Time = Time UTCTime
  deriving (NFData, Eq, Ord, ToJSON, FromJSON, Val)

instance Show Time where
  show = unpack . formatTime "%F"

defaultTime :: Time
defaultTime = Time $ UTCTime (ModifiedJulianDay 0) 0

parseTime :: Text -> Text -> Maybe Time
parseTime f str =
  Time <$> parseTimeM True defaultTimeLocale (unpack f) (unpack str)

formatTime :: Text -> Time -> Text
formatTime f (Time t) = pack $ T.formatTime defaultTimeLocale (unpack f) t

-- usage: addSeconds 600 ...
addSeconds :: NominalDiffTime -> Time -> Time
addSeconds d (Time utc) = Time $ addUTCTime d utc

--

newtype Utf8Text = Utf8Text { unUtf8Text :: Text }

instance Serialize Utf8Text where
    put = put . encodeUtf8 . unUtf8Text
    get = (Utf8Text . decodeUtf8) <$> get

--

type Name = Text

--

data ChangeOp
  = Create
  | Update
  | Delete
  deriving (Generic)

instance ToJSON ChangeOp
instance FromJSON ChangeOp
