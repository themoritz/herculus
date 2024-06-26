{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Aeson.Bson (
  toAeson, aesonifyValue,
  toBson, bsonifyValue,
  ToBSON(..), FromBSON(..),
  decodeValue, eitherDecodeValue
) where

import           Lib.Prelude

import           Data.Aeson             (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types       as Aeson
import qualified Data.Aeson.KeyMap      as Aeson (toList, fromList)
import qualified Data.Aeson.Key         as Aeson (fromText, toText)
import           Data.Bson              (Field ((:=)))
import qualified Data.Bson              as Bson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict    as Map (fromList)
import           Data.Scientific
import           Data.Text              (pack)
import qualified Data.Text.Encoding     as Text
import qualified Data.Vector            as Vector (toList)
import           Numeric

instance ToJSON Bson.Value where
  toJSON = aesonifyValue

-- Compiler thinks this overlaps with `ToJSON a => ToJSON [a]`, but `Bson.Field`
-- has no `ToJSON` instance.
instance {-# OVERLAPPING #-} ToJSON Bson.Document where
  toJSON = Aeson.Object . toAeson

class ToJSON a => ToBSON a where
  toValue :: a -> Bson.Value
  toValue = bsonifyValue . toJSON

class FromJSON a => FromBSON a where
  fromValue :: Bson.Value -> Aeson.Result a
  fromValue = Aeson.fromJSON . aesonifyValue
  fromDocument :: Bson.Document -> Aeson.Result a
  fromDocument = Aeson.fromJSON . Aeson.Object . toAeson

eitherDecodeValue :: FromBSON a => Bson.Value -> Either Text a
eitherDecodeValue v = case fromValue v of
  Aeson.Error   e -> Left $ pack e
  Aeson.Success a -> Right a

decodeValue :: FromBSON a => Bson.Value -> Maybe a
decodeValue = either (const Nothing) Just . eitherDecodeValue

bsonifyValue :: Aeson.Value -> Bson.Value
bsonifyValue (Aeson.Object obj) = Bson.Doc $ toBson obj
bsonifyValue (Aeson.Array array) = Bson.Array $
  map bsonifyValue $ Vector.toList array
bsonifyValue (Aeson.String str) = Bson.String str
bsonifyValue (Aeson.Number n) = case floatingOrInteger n of
  Left f  -> Bson.Float f
  Right i -> Bson.Int64 $ fromIntegral (i :: Integer)
bsonifyValue (Aeson.Bool b) = Bson.Bool b
bsonifyValue Aeson.Null = Bson.Null

aesonifyValue :: Bson.Value -> Aeson.Value
aesonifyValue (Bson.Float f) = toJSON f
aesonifyValue (Bson.String s) = toJSON s
aesonifyValue (Bson.Doc doc) = toJSON doc
aesonifyValue (Bson.Array lst) = toJSON lst
-- TODO: weird: these functions don't preserve Bson's distinction
-- between Bin, Fun, Uuid, Md5, and UserDef and treat them as Text instead
-- Maybe this works because these function are never actually used
aesonifyValue (Bson.Bin (Bson.Binary binary)) = toJSON $ binaryToText binary
aesonifyValue (Bson.Fun (Bson.Function function)) = toJSON $ binaryToText function
aesonifyValue (Bson.Uuid (Bson.UUID uuid)) = toJSON $ binaryToText uuid
aesonifyValue (Bson.Md5 (Bson.MD5 md5)) = toJSON $ binaryToText md5
aesonifyValue (Bson.UserDef (Bson.UserDefined userdef)) = toJSON $ binaryToText userdef
aesonifyValue (Bson.ObjId (Bson.Oid w32 w64)) = toJSON $ showHex w32 (showHex w64 "")
aesonifyValue (Bson.Bool b) = toJSON b
aesonifyValue (Bson.UTC utc) = toJSON utc
aesonifyValue Bson.Null = Aeson.Null
aesonifyValue (Bson.RegEx (Bson.Regex p mods)) = toJSON $
  "/" <> p <> "/" <> mods
aesonifyValue (Bson.JavaScr (Bson.Javascript env code)) =
  toJSON . Map.fromList $
    [ ("environment" :: Text, toJSON env)
    , ("code" :: Text, toJSON code)
    ]
aesonifyValue (Bson.Sym (Bson.Symbol s)) = toJSON s
aesonifyValue (Bson.Int32 int32) = toJSON int32
aesonifyValue (Bson.Int64 int64) = toJSON int64
aesonifyValue (Bson.Stamp (Bson.MongoStamp int64)) = toJSON int64
aesonifyValue (Bson.MinMax mm) = case mm of
  Bson.MinKey -> toJSON (-1 :: Int)
  Bson.MaxKey -> toJSON (1 :: Int)


toBson :: Aeson.Object -> Bson.Document
toBson = map (\(t, v) -> Aeson.toText t := bsonifyValue v) . Aeson.toList

toAeson :: Bson.Document -> Aeson.Object
toAeson = Aeson.fromList . map (\(l := v) -> (Aeson.fromText l, aesonifyValue v))

-- helper

binaryToText :: ByteString -> Text
binaryToText = Text.decodeUtf8 . Base64.encode
