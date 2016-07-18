{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Widgets.Column
  ( CellType
  , ColumnConfig (..)
  , column
  ) where

import Data.Default
import Data.Text (Text, pack, unpack)
import Data.Maybe

import Reflex.Dom

import Api.Rest (loader, api)
import qualified Api.Rest as Api

import           Data.Map        (Map)
import qualified Data.Map as Map

import Lib

data ColumnConfig t = ColumnConfig
  { _columnConfig_setType :: Event t ColumnType
  , _columnConfig_setName :: Event t Text
  , _columnConfig_initialType :: ColumnType
  , _columnConfig_initialName :: Text
  }

instance Reflex t => Default (ColumnConfig t) where
  def = ColumnConfig
    { _columnConfig_setType = never
    , _columnConfig_setName = never
    , _columnConfig_initialType = ColumnInput DataString
    , _columnConfig_initialName = ""
    }

data CellType
  = CTInput
  | CTDerived
  deriving (Eq, Ord, Show, Read)

toCellType :: ColumnType -> CellType
toCellType (ColumnInput _) = CTInput
toCellType (ColumnDerived _) = CTDerived

toDataType :: ColumnType -> Maybe DataType
toDataType (ColumnInput t) = Just t
toDataType _ = Nothing

toExpr :: ColumnType -> Maybe String
toExpr (ColumnInput _) = Nothing
toExpr (ColumnDerived t) = Just $ unpack t

cellTypeEntries :: Map CellType String
cellTypeEntries = Map.fromList
  [ (CTInput, "Input")
  , (CTDerived, "Derived")
  ]

dataTypeEntries :: Map DataType String
dataTypeEntries = Map.fromList
 [ (DataBoolean, "Bool")
 , (DataString, "String")
 , (DataNumber, "Number")
 , (DataRecord, "Record")
 ]

column :: forall t m. MonadWidget t m => Id Column -> ColumnConfig t -> m ()
column columnId (ColumnConfig setType setName initialType initialName) = el "div" $ do
  name <- (fmap pack . current . _textInput_value) <$> textInput
            def { _textInputConfig_setValue = unpack <$> setName
                , _textInputConfig_initialValue = unpack initialName
                }
  nameTrigger <- button "Set Name"
  _ <- loader (Api.columnSetName api (constant $ Right columnId) (Right <$> name)) nameTrigger
  selectCellType <- dropdown (toCellType initialType)
                             (constDyn cellTypeEntries) $
                             (def :: DropdownConfig t CellType)
                               { _dropdownConfig_setValue =
                                     (toCellType <$> setType)
                               }
  selectDataType <- dropdown (fromMaybe DataString $ toDataType initialType)
                             (constDyn dataTypeEntries)
                             (def :: DropdownConfig t DataType)
                               { _dropdownConfig_setValue =
                                   fmapMaybe toDataType setType
                               }
  expr <- (fmap pack . current . _textInput_value) <$> textInput
            def { _textInputConfig_setValue = fmapMaybe toExpr setType
                , _textInputConfig_initialValue = fromMaybe "" $ toExpr initialType
                }
  typeTrigger <- button "Set Type"
  let columnType = do
        cellType <- current $ _dropdown_value selectCellType
        case cellType of
          CTInput   -> ColumnInput <$> (current $ _dropdown_value selectDataType)
          CTDerived -> ColumnDerived <$> expr
  _ <- loader (Api.columnSetType api (constant $ Right columnId) (Right <$> columnType)) typeTrigger
  pure ()
