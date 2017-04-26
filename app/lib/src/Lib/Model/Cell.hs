{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Model.Cell where

import           Lib.Prelude

import           Control.Lens
import           Control.Monad.Writer

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Bson                    (Val, (=:))
import qualified Data.Bson                    as Bson

import           Text.PrettyPrint.Leijen.Text hiding ((<$>))

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

data CellContent
  = CellValue Value
  | CellError Text
  deriving (Eq, Show, Typeable, Generic)

cellContentDoc :: CellContent -> Doc
cellContentDoc = \case
  CellValue v -> valueDoc v
  CellError e -> textStrict "Error:" <+> textStrict e

instance ToJSON CellContent
instance FromJSON CellContent

instance ToBSON CellContent
instance FromBSON CellContent

instance Val CellContent where
  val = toValue
  cast' = decodeValue

--

data Value
  = VString Text
  | VNumber Number
  | VInteger Integer
  | VTime Time
  | VRowRef (Maybe (Id Row))
  | VData Text [Value]
  | VRecord [(Text, Value)] -- List better than Map for serialization
  -- Special support
  | VBool Bool
  | VList [Value]
  | VMaybe (Maybe Value)
  deriving (Show, Generic, Typeable, Eq)

instance ToJSON Value
instance FromJSON Value

valueDoc :: Value -> Doc
valueDoc = error "not defined"

-- TODO: delete in favor of ajax calls to the server version of defaultContent
defaultContentPure :: DataType -> Value
defaultContentPure = \case
  DataBool     -> VBool False
  DataString   -> VString ""
  DataNumber   -> VNumber 0
  DataTime     -> VTime defaultTime
  DataRowRef _ -> VRowRef Nothing
  DataList   _ -> VList []
  DataMaybe  _ -> VMaybe Nothing

-- | Returns `Nothing` if no reference had to be invalidated
invalidateRowRef :: Id Row -> Value -> Maybe Value
invalidateRowRef r old =
    let (new, invalidated) = runWriter (go old)
    in if length invalidated == 0
         then Nothing
         else Just new
  where
    go :: Value -> Writer [()] Value
    go = \case
      VBool b    -> pure $ VBool b
      VString s  -> pure $ VString s
      VNumber n  -> pure $ VNumber n
      VTime t    -> pure $ VTime t
      VRowRef mr -> VRowRef <$> if mr == Just r
                                  then do tell [()]
                                          pure Nothing
                                  else pure mr
      VList vs   -> VList <$> mapM go vs
      VMaybe mv  -> VMaybe <$> mapM go mv

--

data Cell = Cell
  { _cellContent  :: CellContent
  , _cellTableId  :: Id Table
  , _cellColumnId :: Id Column
  , _cellRowId    :: Id Row
  } deriving (Generic)

makeLenses ''Cell

newCell :: Id Table -> Id Column -> Id Row -> CellContent -> Cell
newCell t c r content = Cell content t c r

instance Model Cell where collectionName = const "cells"

instance ToJSON Cell
instance FromJSON Cell

instance ToDocument Cell where
  toDocument (Cell con t c r) =
    [ "content"  =: con
    , "tableId"  =: toObjectId t
    , "columnId" =: toObjectId c
    , "rowId"    =: toObjectId r
    ]

instance FromDocument Cell where
  parseDocument doc =
    Cell <$> Bson.lookup "content"  doc
         <*> (fromObjectId <$> Bson.lookup "tableId"  doc)
         <*> (fromObjectId <$> Bson.lookup "columnId" doc)
         <*> (fromObjectId <$> Bson.lookup "rowId"    doc)
