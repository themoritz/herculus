{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Util.Base64
  ( Base64
  , unBase64
  , mkBase64
  , toBase64
  , toBase64Unsafe
  , Base64Url
  , unBase64Url
  , mkBase64Url
  , toBase64Url
  , toBase64UrlUnsafe
  ) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Bson                  as Bson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           GHC.Generics               (Generic)
import           Web.HttpApiData            (FromHttpApiData (..),
                                             ToHttpApiData (..))

-- | a wrapper around ByteString that only holds
--   valid base64-encoded bytestrings if used correctly
newtype Base64 = Base64 { unBase64 :: ByteString }
  deriving (Generic, Show, Eq)

instance Bson.Val Base64 where
  val = Bson.val . Text.decodeUtf8 . unBase64
  cast' = fmap (Base64 . Text.encodeUtf8) . Bson.cast'

instance FromJSON Base64 where
  parseJSON = fmap (Base64 . Text.encodeUtf8) . parseJSON

instance ToJSON Base64 where
  toJSON = toJSON . Text.decodeUtf8 . unBase64

instance ToHttpApiData Base64 where
  toUrlPiece = toUrlPiece . Text.decodeUtf8 . unBase64
  toHeader = unBase64
  toQueryParam = toQueryParam . Text.decodeUtf8 . unBase64

instance FromHttpApiData Base64 where
  -- :: Text -> Either Text Base64
  parseUrlPiece txt = do
    txt' <- parseUrlPiece txt
    let bs = Text.encodeUtf8 txt'
    toBase64 bs

  -- :: ByteString -> Either Text Base64
  parseHeader = toBase64

  -- :: Text -> Either Text Base64
  parseQueryParam txt = do
    txt' <- parseQueryParam txt
    let bs = Text.encodeUtf8 txt'
    toBase64 bs

-- safely build a base64 object by base64 encoding a bytestring
mkBase64 :: ByteString -> Base64
mkBase64 = Base64 . Base64.encode

-- unsafely build a base64 object, use only when the bytestring
-- is a valid base64 encoded object already
toBase64Unsafe :: ByteString -> Base64
toBase64Unsafe = Base64

-- | the safe variant of toBase64Unsafe
--   Build a base64 object from a base64-encoded bytestring.
--   Fail if the object is not valid base64.
toBase64 :: ByteString -> Either Text Base64
toBase64 bs = case Base64.decode bs of
  Left err -> Left $ Text.pack err
  Right _  -> Right $ Base64 bs

-- | a wrapper around ByteString that only holds
--   valid base64-url-encoded bytestrings if used correctly
newtype Base64Url = Base64Url { unBase64Url :: ByteString }
  deriving (Generic, Show, Eq)

instance Bson.Val Base64Url where
  val = Bson.val . Text.decodeUtf8 . unBase64Url
  cast' = fmap (Base64Url . Text.encodeUtf8) . Bson.cast'

instance FromJSON Base64Url where
  parseJSON = fmap (Base64Url . Text.encodeUtf8) . parseJSON

instance ToJSON Base64Url where
  toJSON = toJSON . Text.decodeUtf8 . unBase64Url

instance ToHttpApiData Base64Url where
  toUrlPiece = toUrlPiece . Text.decodeUtf8 . unBase64Url
  toHeader = unBase64Url
  toQueryParam = toQueryParam . Text.decodeUtf8 . unBase64Url

instance FromHttpApiData Base64Url where
  -- :: Text -> Either Text Base64
  parseUrlPiece txt = do
    txt' <- parseUrlPiece txt
    let bs = Text.encodeUtf8 txt'
    toBase64Url bs

  -- :: ByteString -> Either Text Base64
  parseHeader = toBase64Url

  -- :: Text -> Either Text Base64
  parseQueryParam txt = do
    txt' <- parseQueryParam txt
    let bs = Text.encodeUtf8 txt'
    toBase64Url bs

-- safely build a base64 object by base64 encoding a bytestring
mkBase64Url :: ByteString -> Base64Url
mkBase64Url = Base64Url . Base64URL.encode

-- unsafely build a base64 object, use only when the bytestring
-- is a valid base64 encoded object already
toBase64UrlUnsafe :: ByteString -> Base64Url
toBase64UrlUnsafe = Base64Url

-- | the safe variant of toBase64Unsafe
--   Build a base64 object from a base64-encoded bytestring.
--   Fail if the object is not valid base64.
toBase64Url :: ByteString -> Either Text Base64Url
toBase64Url bs = case Base64URL.decode bs of
  Left err -> Left $ Text.pack err
  Right _  -> Right $ Base64Url bs
