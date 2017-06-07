{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Model.Cell where

import           Lib.Prelude

import           Control.Lens
import           Control.Monad.Writer

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Bson            (Val, (=:))
import qualified Data.Bson            as Bson

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

data CellContent
  = CellValue Value
  | CellError Text
  deriving (Eq, Show, Typeable, Generic, ToJSON, FromJSON)

instance ToBSON CellContent
instance FromBSON CellContent

instance Val CellContent where
  val = toValue
  cast' = decodeValue

--

data Value
  = VUndefined
  | VString Text
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
  deriving (Show, Generic, Typeable, Eq, ToJSON, FromJSON)

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
      VUndefined -> pure VUndefined
      VBool b    -> pure $ VBool b
      VString s  -> pure $ VString s
      VNumber n  -> pure $ VNumber n
      VInteger i -> pure $ VInteger i
      VTime t    -> pure $ VTime t
      VRowRef mr -> VRowRef <$> if mr == Just r
                                  then do tell [()]
                                          pure Nothing
                                  else pure mr
      VList vs   -> VList <$> mapM go vs
      VMaybe mv  -> VMaybe <$> mapM go mv
      VData l vs -> VData l <$> mapM go vs
      VRecord m  -> VRecord <$> mapM (\(k, v) -> (k,) <$> go v) m

--

data Cell = Cell
  { _cellContent  :: CellContent
  , _cellTableId  :: Id Table
  , _cellColumnId :: Id Column
  , _cellRowId    :: Id Row
  } deriving (Generic, ToJSON, FromJSON)

makeLenses ''Cell

newCell :: Id Table -> Id Column -> Id Row -> CellContent -> Cell
newCell t c r content = Cell content t c r

instance Model Cell where collectionName = const "cells"

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
