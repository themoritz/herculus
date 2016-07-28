{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Aeson.Bson (
  toAeson, aesonifyValue,
  toBson, bsonifyValue,
  ToBSON(..), FromBSON(..),
  decodeValue, eitherDecodeValue
) where

import           Data.Monoid ((<>))
import           Data.Aeson.Types       as Aeson
import           Data.Bson              as Bson
import           Data.HashMap.Strict    as Map (fromList, toList)
import           Data.Scientific
import           Data.Text              as T hiding (map)
import           Data.Vector            as Vector (toList)
import           Numeric

import           Lib.Base64

instance ToJSON Bson.Value where
  toJSON = aesonifyValue

instance ToJSON Document where
  toJSON = Object . toAeson

class ToJSON a => ToBSON a where
  toValue :: a -> Bson.Value
  toValue = bsonifyValue . toJSON

class FromJSON a => FromBSON a where
  fromValue :: Bson.Value -> Aeson.Result a
  fromValue = fromJSON . aesonifyValue
  fromDocument :: Bson.Document -> Aeson.Result a
  fromDocument = fromJSON . Aeson.Object . toAeson

eitherDecodeValue :: FromBSON a => Bson.Value -> Either String a
eitherDecodeValue v = case fromValue v of
  Error   e -> Left e
  Success a -> Right a

decodeValue :: FromBSON a => Bson.Value -> Maybe a
decodeValue = either (const Nothing) Just . eitherDecodeValue

bsonifyValue :: Aeson.Value -> Bson.Value
bsonifyValue (Object obj) = Doc $ toBson obj
bsonifyValue (Aeson.Array array) = Bson.Array $
  map bsonifyValue $ Vector.toList array
bsonifyValue (Aeson.String str) = Bson.String str
bsonifyValue (Number n) = case floatingOrInteger n of
  Left f -> Float f
  Right i -> Int64 $ fromIntegral i
bsonifyValue (Aeson.Bool b) = Bson.Bool b
bsonifyValue Aeson.Null = Bson.Null

aesonifyValue :: Bson.Value -> Aeson.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (Bson.String s) = toJSON s
aesonifyValue (Doc doc) = toJSON doc
aesonifyValue (Bson.Array list) = toJSON list
aesonifyValue (Bin (Binary binary)) = toJSON $ Base64 binary
aesonifyValue (Fun (Function function)) = toJSON $ Base64 function
aesonifyValue (Uuid (UUID uuid)) = toJSON $ Base64 uuid
aesonifyValue (Md5 (MD5 md5)) = toJSON $ Base64 md5
aesonifyValue (UserDef (UserDefined userdef)) = toJSON $ Base64 userdef
aesonifyValue (ObjId (Oid w32 w64)) = toJSON $ showHex w32 (showHex w64 "")
aesonifyValue (Bson.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue Bson.Null = Aeson.Null
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


toBson :: Aeson.Object -> Bson.Document
toBson = map (\(t, v) -> (t := bsonifyValue v)) . Map.toList

toAeson :: Bson.Document -> Aeson.Object
toAeson = Map.fromList . map (\(l := v) -> (l, aesonifyValue v))
