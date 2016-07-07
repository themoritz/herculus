{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Aeson.Bson (
  toAeson, aesonifyValue,
  toBson, bsonifyValue,
  ToValue(..), FromValue(..),
  decodeValue, eitherDecodeValue
) where

import           Data.Monoid ((<>))
import           Data.Aeson.Types       as AESON
import           Data.Bson              as BSON
import           Data.HashMap.Strict    as Map (fromList, toList)
import           Data.Scientific
import           Data.Text              as T hiding (map)
import           Data.Vector            as Vector (toList)
import           Numeric

import           Lib.Base64

instance ToJSON BSON.Value where
  toJSON = aesonifyValue

instance ToJSON Document where
  toJSON = Object . toAeson

class ToJSON a => ToValue a where
  toValue :: a -> BSON.Value
  toValue = bsonifyValue . toJSON

class FromJSON a => FromValue a where
  fromValue :: BSON.Value -> AESON.Result a
  fromValue = fromJSON . aesonifyValue

eitherDecodeValue :: FromValue a => BSON.Value -> Either String a
eitherDecodeValue v = case fromValue v of
  Error   e -> Left e
  Success a -> Right a

decodeValue :: FromValue a => BSON.Value -> Maybe a
decodeValue = either (const Nothing) Just . eitherDecodeValue

bsonifyValue :: AESON.Value -> BSON.Value
bsonifyValue (Object obj) = Doc $ toBson obj
bsonifyValue (AESON.Array array) = BSON.Array $
  map bsonifyValue $ Vector.toList array
bsonifyValue (AESON.String str) = BSON.String str
bsonifyValue (Number n) = case floatingOrInteger n of
  Left f -> Float f
  Right i -> Int64 $ fromIntegral i
bsonifyValue (AESON.Bool b) = BSON.Bool b
bsonifyValue AESON.Null = BSON.Null

aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (BSON.String s) = toJSON s
aesonifyValue (Doc doc) = toJSON doc
aesonifyValue (BSON.Array list) = toJSON list
aesonifyValue (Bin (Binary binary)) = toJSON $ Base64 binary
aesonifyValue (Fun (Function function)) = toJSON $ Base64 function
aesonifyValue (Uuid (UUID uuid)) = toJSON $ Base64 uuid
aesonifyValue (Md5 (MD5 md5)) = toJSON $ Base64 md5
aesonifyValue (UserDef (UserDefined userdef)) = toJSON $ Base64 userdef
aesonifyValue (ObjId (Oid w32 w64)) = toJSON $ showHex w32 (showHex w64 "")
aesonifyValue (BSON.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue BSON.Null = AESON.Null
aesonifyValue (RegEx (Regex p mods)) = toJSON $
  "/" <> T.unpack p <> "/" <> T.unpack mods
aesonifyValue (JavaScr (Javascript env code)) =
  toJSON . Map.fromList $
    [ (T.pack "environment", toJSON env)
    , (T.pack "code", toJSON code)
    ]
aesonifyValue (Sym (Symbol sym)) = toJSON sym
aesonifyValue (Int32 int32) = toJSON int32
aesonifyValue (Int64 int64) = toJSON int64
aesonifyValue (Stamp (MongoStamp int64)) = toJSON int64
aesonifyValue (MinMax mm) = case mm of
  MinKey -> toJSON (-1 :: Int)
  MaxKey -> toJSON (1 :: Int)


toBson :: AESON.Object -> BSON.Document
toBson = map (\(t, v) -> (t := bsonifyValue v)) . Map.toList

toAeson :: BSON.Document -> AESON.Object
toAeson = Map.fromList . map (\(l := v) -> (l, aesonifyValue v))
