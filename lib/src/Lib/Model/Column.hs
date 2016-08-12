{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}

module Lib.Model.Column where

import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               Value (..), object, (.:), (.=))
import           Data.Aeson.Bson
import           Data.Bson                    (Val, (=:))
import qualified Data.Bson                    as Bson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as Base64
import           Data.Monoid                  ((<>))
import           Data.Serialize
import           Data.Text                    (Text, pack)
import           Data.Text.Encoding

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Dependencies.Types
import           Lib.Model.Types
import           Lib.Types

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

data Column = Column
  { columnTableId       :: Id Table
  , columnName          :: Text
  , columnDataType      :: DataType
  , columnInputType     :: InputType
  , columnSourceCode    :: Text
  , columnCompileResult :: CompileResult
  } deriving (Eq, Generic)

instance Model Column where
  collectionName = const "columns"

instance ToJSON Column
instance FromJSON Column

instance ToDocument Column where
  toDocument (Column t name typ inputType source compiled) =
    [ "tableId" =: toObjectId t
    , "name" =: name
    , "type" =: toValue typ
    , "inputType" =: toValue inputType
    , "sourceCode" =: source
    , "compiledCode" =: compiled
    ]

instance FromDocument Column where
  parseDocument doc = do
    t <- Bson.lookup "tableId" doc
    name <- Bson.lookup "name" doc
    typVal <- Bson.lookup "type" doc
    inpTypeVal <- Bson.lookup "inputType" doc
    source <- Bson.lookup "sourceCode" doc
    compiled <- Bson.lookup "compiledCode" doc
    case eitherDecodeValue typVal of
      Right typ -> case eitherDecodeValue inpTypeVal of
        Right inpTyp -> pure $ Column (fromObjectId t) name typ inpTyp source compiled
        Left msg -> Left $ pack msg
      Left msg -> Left $ pack msg

data CompileResult
  = CompileResultCode (Expr Id)
  | CompileResultNone
  | CompileResultError Text
  deriving (Eq, Show, Generic)

instance ToJSON CompileResult
instance FromJSON CompileResult

instance ToBSON CompileResult
instance FromBSON CompileResult

instance Val CompileResult where
  val = toValue
  cast' = decodeValue

data InputType
  = ColumnInput
  | ColumnDerived
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON InputType
instance FromJSON InputType

instance ToBSON InputType
instance FromBSON InputType

instance Val InputType where
  val = toValue
  cast' = decodeValue

--

type Name = String

data Expr v
  = Lam Name (Expr v)
  | App (Expr v) (Expr v)
  | Let Name (Expr v) (Expr v)
  -- | Fix Expr
  | If (Expr v) (Expr v) (Expr v)
  | Var Name
  | Lit Lit
  | Binop Binop (Expr v) (Expr v)
  | PrjRecord (Expr v) (Ref Column)
  --
  | ColumnRef (v Column)
  | ColumnOfTableRef (v Table) (v Column)
  | TableRef (v Table)
  deriving (Generic)

deriving instance Eq (Expr Ref)
deriving instance Eq (Expr Id)

deriving instance Show (Expr Ref)
deriving instance Show (Expr Id)

instance ToJSON (Expr Id)
instance FromJSON (Expr Id)

data Lit
  = LNumber Number
  | LBool Bool
  | LString Text
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Lit
instance FromJSON Lit

data Binop = Add | Sub | Mul
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Binop
instance FromJSON Binop

--

collectDependencies :: Expr Id -> [(Id Column, DependencyType)]
collectDependencies expr = go expr
  where go e = case e of
          Lam _ body           -> go body
          App f e              -> go f <> go e
          Let _ e body         -> go e <> go body
          If c t e             -> go c <> go t <> go e
          Var _                -> []
          Lit _                -> []
          Binop _ l r          -> go l <> go r
          PrjRecord e _        -> go e
          ColumnRef c          -> [(c, OneToOne)]
          ColumnOfTableRef _ c -> [(c, OneToAll)]
          TableRef t           -> undefined
