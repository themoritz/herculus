{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Widgets.Column
  ( ColumnConfig (..)
  , column
  ) where

import Data.Text (Text, pack, unpack)
import Data.Maybe

import Reflex.Dom

import Api.Rest (loader, api)
import qualified Api.Rest as Api

import           Data.Map        (Map)
import qualified Data.Map as Map

import Lib.Types
import Lib.Model.Types

data ColumnConfig t = ColumnConfig
  { _columnConfig_set :: Event t Column
  , _columnConfig_initial :: Column
  }

cellTypeEntries :: Map ColumnType String
cellTypeEntries = Map.fromList
  [ (ColumnInput, "Input")
  , (ColumnDerived, "Derived")
  ]

dataTypeEntries :: Map DataType String
dataTypeEntries = Map.fromList
 [ (DataBoolean, "Bool")
 , (DataString, "String")
 , (DataNumber, "Number")
 , (DataRecord, "Record")
 ]

column :: forall t m. MonadWidget t m
       => Id Table -> Id Column
       -> ColumnConfig t -> m (Event t Column)
column tableId columnId (ColumnConfig set initial) = el "div" $ do
  name <- (fmap pack . current . _textInput_value) <$> textInput
            def { _textInputConfig_setValue = unpack . columnName <$> set
                , _textInputConfig_initialValue = unpack . columnName $ initial
                }
  (selectCellType, selectDataType) <-
    divClass "row" $ do
      ct <- dropdown (columnInputType initial)
                     (constDyn cellTypeEntries) $
                     (def :: DropdownConfig t ColumnType)
                       { _dropdownConfig_setValue =
                             (columnInputType <$> set)
                       }
      dt <- dropdown (columnType initial)
                     (constDyn dataTypeEntries)
                     (def :: DropdownConfig t DataType)
                       { _dropdownConfig_setValue =
                           (columnType <$> set)
                       }
      pure (ct, dt)
  expr <- (fmap pack . current . _textInput_value) <$> textInput
            (def :: TextInputConfig t)
                { _textInputConfig_setValue = unpack . columnExpression <$> set
                , _textInputConfig_initialValue = unpack . columnExpression $ initial
                , _textInputConfig_attributes = constDyn ("style" =: "width: 160px")
                }
  trigger <- button "Set"
  let column = do
        nm <- name
        columnType <- current $ _dropdown_value selectCellType
        dataType <- current $ _dropdown_value selectDataType
        expression <- expr
        pure $ Column tableId nm dataType columnType expression
  _ <- loader (Api.columnUpdate api (constant $ Right columnId) (Right <$> column)) trigger
  pure $ tag column trigger
