{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module Lib.Model.Column where

import           Control.DeepSeq

import           Data.Aeson                   (FromJSON (..), ToJSON (..))
import           Data.Aeson.Bson
import           Data.Bson                    (Val, (=:))
import qualified Data.Bson                    as Bson
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text, pack)

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Dependencies.Types
import           Lib.Model.Types
import           Lib.Types

data DataType
  = DataBool
  | DataString
  | DataNumber
  | DataTime
  | DataRecord (Id Table)
  | DataList DataType
  | DataMaybe DataType
  deriving (Eq, Ord, Show, Read, Generic, NFData)

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
  } deriving (Eq, Generic, NFData)

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
  = CompileResultCode TExpr
  | CompileResultNone
  | CompileResultError Text
  deriving (Eq, Show, Generic, NFData)

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
  deriving (Eq, Ord, Show, Read, Generic, NFData)

instance ToJSON InputType
instance FromJSON InputType

instance ToBSON InputType
instance FromBSON InputType

instance Val InputType where
  val = toValue
  cast' = decodeValue

--

type Name = String

data Lit
  = LNumber Number
  | LBool Bool
  | LString Text
  deriving (Show, Eq, Ord, Generic, NFData)

instance ToJSON Lit
instance FromJSON Lit

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | LessEq
  | GreaterEq
  | Less
  | Greater
  | And
  | Or
  deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON Binop
instance FromJSON Binop

data PExpr
  = PLam Name (PExpr)
  | PApp (PExpr) (PExpr)
  | PLet Name (PExpr) (PExpr)
  -- | PFix Expr
  | PIf (PExpr) (PExpr) (PExpr)
  | PVar Name
  | PLit Lit
  | PBinop Binop (PExpr) (PExpr)
  | PPrjRecord (PExpr) (Ref Column)
  --
  | PColumnRef (Ref Column)
  | PColumnOfTableRef (Ref Table) (Ref Column)
  | PTableRef (Ref Table)
  deriving (Eq, Show, Generic)

instance ToJSON PExpr
instance FromJSON PExpr

data TExpr
  = TLam Name (TExpr)
  | TApp (TExpr) (TExpr)
  | TLet Name (TExpr) (TExpr)
  --T | Fix Expr
  | TIf (TExpr) (TExpr) (TExpr)
  | TVar Name
  | TLit Lit
  | TBinop Binop (TExpr) (TExpr)
  | TPrjRecord (TExpr) (Ref Column)
  --
  | TColumnRef (Id Column)
  | TWholeColumnRef (Id Column)
  | TTableRef (Id Table) [Id Column]
  deriving (Eq, Show, Generic, NFData)

instance ToJSON TExpr
instance FromJSON TExpr

--

collectDependencies :: TExpr -> [(Id Column, DependencyType)]
collectDependencies expr = go expr
  where go e' = case e' of
          TLam _ body       -> go body
          TApp f e          -> go f <> go e
          TLet _ e body     -> go e <> go body
          TIf c t e         -> go c <> go t <> go e
          TVar _            -> []
          TLit _            -> []
          TBinop _ l r      -> go l <> go r
          TPrjRecord e _    -> go e
          TColumnRef c      -> [(c, OneToOne)]
          TWholeColumnRef c -> [(c, OneToAll)]
          TTableRef _ cs    -> map (,OneToAll) cs
