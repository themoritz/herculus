module Lib.Base64 where

import           Data.Aeson

import           Data.ByteString
import           Data.ByteString.Base64 as B64
import           Data.Text.Encoding     as TE

newtype Base64 = Base64 ByteString

instance ToJSON Base64 where
  toJSON (Base64 bs) = toJSON . TE.decodeUtf8 . B64.encode $ bs

instance FromJSON Base64 where
  parseJSON jsn = (Base64 . B64.decodeLenient . TE.encodeUtf8 ) <$> parseJSON jsn
