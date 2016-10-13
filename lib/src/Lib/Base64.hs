{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Base64
  ( Base64
  , mkBase64
  , mkBase64'
  , mkBase64Unsafe
  , unBase64
  ) where

import           Control.DeepSeq        (NFData)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import qualified Data.Bson              as Bson
import           Data.ByteString        (ByteString)
import           Data.ByteString.Base64 as Base64
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding     as Text

-- | a wrapper around ByteString that only holds
--   valid base64-encoded bytestrings if used correctly
newtype Base64 = Base64 { unBase64 :: ByteString }
  deriving (NFData, Show, Eq)

instance Bson.Val Base64 where
  val = Bson.val . Text.decodeUtf8 . unBase64
  cast' = fmap (Base64 . Text.encodeUtf8) . Bson.cast'

instance FromJSON Base64 where
  parseJSON = fmap (Base64 . Text.encodeUtf8) . parseJSON

instance ToJSON Base64 where
  toJSON = toJSON . Text.decodeUtf8 . unBase64

-- safely build a base64 object by base64 encoding a bytestring
mkBase64 :: ByteString -> Base64
mkBase64 = Base64 . Base64.encode

-- unsafely build a base64 object, use only when the bytestring
-- is a valid base64 encoded object already
mkBase64Unsafe :: ByteString -> Base64
mkBase64Unsafe = Base64

-- | the safe variant of mkBase64Unsafe
--   Build a base64 object from a base64-encoded bytestring.
--   Fail if the object is not valid base64.
mkBase64' :: ByteString -> Either Text Base64
mkBase64' bs = case Base64.decode bs of
  Left err -> Left $ Text.pack err
  Right _  -> Right $ Base64 bs
