{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Lib.Model.Common where

import           Lib.Prelude

import           Data.Aeson
import           Data.Aeson.Bson
import           Data.Bson          (Val (..))

import           Lib.Compiler.Error

data CompileResult a
  = CompileResultOk a
  | CompileResultNone
  | CompileResultError [Error]
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToJSON a => ToBSON (CompileResult a)
instance FromJSON a => FromBSON (CompileResult a)

instance (Typeable a, ToJSON a, FromJSON a, Show a, Eq a) => Val (CompileResult a) where
  val = toValue
  cast' = decodeValue
