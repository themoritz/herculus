{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Lib.Model.Cell where

import           Control.DeepSeq
import           Control.Monad.Writer

import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Bson        (Val, (=:))
import qualified Data.Bson        as Bson
import           Data.Text        (Text, unpack)
import           Data.Typeable

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Types
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
  | VRecord (Maybe (Id Record))
  | VList [Value]
  | VMaybe (Maybe Value)
  deriving (Generic, NFData, Typeable, Eq)

instance Show Value where
  show = \case
    VBool b -> show b
    VString t -> show t
    VNumber n -> show n
    VTime t -> show t
    VRecord i -> "Record " ++ show i
    VList vs -> show vs
    VMaybe m -> show m

instance ToJSON Value
instance FromJSON Value

-- TODO: delete in favor of ajax calls to the server version of defaultContent

defaultContentPure :: DataType -> CellContent
defaultContentPure = \case
  DataBool     -> CellValue $ VBool False
  DataString   -> CellValue $ VString ""
  DataNumber   -> CellValue $ VNumber 0
  DataTime     -> CellValue $ VTime $ defaultTime
  DataRecord _ -> CellError "no default for records"
  DataList   _ -> CellValue $ VList []
  DataMaybe  _ -> CellValue $ VMaybe Nothing--

-- | Returns `Nothing` if no record had to be invalidated
invalidateRecord :: Id Record -> Value -> Maybe Value
invalidateRecord r old =
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
      VRecord mr -> VRecord <$> if mr == Just r
                                  then do tell [()]
                                          pure Nothing
                                  else pure mr
      VList vs   -> VList <$> mapM go vs
      VMaybe mv  -> VMaybe <$> mapM go mv

--

data Aspects = Aspects
  { aspectsTableId  :: Id Table
  , aspectsColumnId :: Id Column
  , aspectsRecordId :: Id Record
  } deriving (Generic, NFData, Eq, Ord, Show, Typeable)

instance ToJSON Aspects
instance FromJSON Aspects

instance Val Aspects where
  val (Aspects t c r) = Bson.Doc
    [ "tableId" =: toObjectId t
    , "columnId" =: toObjectId c
    , "recordId" =: toObjectId r
    ]
  cast' (Bson.Doc doc) = do
    t <- Bson.lookup "tableId" doc
    c <- Bson.lookup "columnId" doc
    r <- Bson.lookup "recordId" doc
    pure $ Aspects t c r
  cast' _ = fail "expected document"

data Cell = Cell
  { cellContent :: CellContent
  , cellAspects :: Aspects
  } deriving (Generic, NFData)

newCell :: Id Table -> Id Column -> Id Record -> CellContent -> Cell
newCell t c r content = Cell
  { cellContent = content
  , cellAspects = Aspects t c r
  }

instance Model Cell where collectionName = const "cells"

instance ToJSON Cell
instance FromJSON Cell

instance ToDocument Cell where
  toDocument (Cell con asp) =
    [ "content" =: con
    , "aspects" =: asp
    ]

instance FromDocument Cell where
  parseDocument doc = Cell <$> Bson.lookup "content" doc
                           <*> Bson.lookup "aspects" doc
