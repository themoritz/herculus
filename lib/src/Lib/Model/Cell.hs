{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Model.Cell where

import           Control.DeepSeq
import           Control.Monad.Writer

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Bson            (Val, (=:))
import qualified Data.Bson            as Bson
import           Data.Text            (Text, unpack)
import           Data.Typeable

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

data CellContent
  = CellValue Value
  | CellError Text
  deriving (Eq, NFData, Typeable, Generic)

instance Show CellContent where
  show = \case
    CellValue v -> show v
    CellError e -> "Error: " ++ unpack e

instance ToJSON CellContent
instance FromJSON CellContent

instance ToBSON CellContent
instance FromBSON CellContent

instance Val CellContent where
  val = toValue
  cast' = decodeValue

--

data Value
  = VBool Bool
  | VString Text
  | VNumber Number
  | VTime Time
  | VRowRef (Maybe (Id Row))
  | VList [Value]
  | VMaybe (Maybe Value)
  deriving (Generic, NFData, Typeable, Eq)

instance Show Value where
  show = \case
    VBool b   -> show b
    VString t -> show t
    VNumber n -> show n
    VTime t   -> show t
    VRowRef i -> "RowRef " ++ show i
    VList vs  -> show vs
    VMaybe m  -> show m

instance ToJSON Value
instance FromJSON Value

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
  { cellContent  :: CellContent
  , cellTableId  :: Id Table
  , cellColumnId :: Id Column
  , cellRowId    :: Id Row
  } deriving (Generic, NFData, Show)

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
