{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Widgets.Column
  ( ColumnConfig (..)
  , column
  ) where

import Control.Monad (void)

import Data.Text (unpack)
import Data.Monoid

import Reflex.Dom

import Api.Rest (loader, loader', api)
import qualified Api.Rest as Api

import           Data.Map        (Map)
import qualified Data.Map as Map

import Lib.Types
import Lib.Model.Types
import Lib.Model.Column

import Misc

data ColumnConfig t = ColumnConfig
  { _columnConfig_set :: Event t Column
  , _columnConfig_initial :: Column
  }

inputTypeEntries :: Map InputType String
inputTypeEntries = Map.fromList
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

  let columnIdArg = constant $ Right columnId

  name <- textInputT def
            { _textInputConfig_setValue = unpack . columnName <$> set
            , _textInputConfig_initialValue = unpack . columnName $ initial
            }
  nameSet <- button "Set"
  loader' (Api.columnSetName api columnIdArg (Right <$> current name)) nameSet

  it <- dropdown (columnInputType initial)
                 (constDyn inputTypeEntries) $
                 (def :: DropdownConfig t InputType)
                   { _dropdownConfig_setValue =
                         (columnInputType <$> set)
                   }
  setIt <- button "Set"

  dt <- dropdown (columnDataType initial)
                 (constDyn dataTypeEntries)
                 (def :: DropdownConfig t DataType)
                   { _dropdownConfig_setValue =
                       (columnDataType <$> set)
                   }
  setDt <- button "Set"

  let inputType = _dropdown_value it
      dataType = _dropdown_value dt

  loader' (Api.columnSetInputType api columnIdArg (Right <$> current inputType))
          setIt

  loader' (Api.columnSetDataType api columnIdArg (Right <$> current dataType))
          setDt

  source <- textInputT (def :: TextInputConfig t)
              { _textInputConfig_setValue = unpack . columnSourceCode <$> set
              , _textInputConfig_initialValue = unpack . columnSourceCode $ initial
              , _textInputConfig_attributes = constDyn ("style" =: "width: 160px")
              }
  sourceSet <- button "Set"

  compiledE <- loader (Api.columnSetSourceCode api columnIdArg
                                                  (Right <$> current source))
                      sourceSet

  compiledD <- holdDyn (columnCompiledCode initial) $ fmapMaybe id compiledE

  void $ dynWidget compiledD $ \case
    CompiledCode _        -> text "Ok"
    CompiledCodeNone      -> pure ()
    CompiledCodeError msg -> text $ "Error: " <> unpack msg

  let columnB = Column <$> pure tableId
                       <*> current name
                       <*> current dataType
                       <*> current inputType
                       <*> current source
                       <*> current compiledD

  pure $ tag columnB $ leftmost [ nameSet, setIt, setDt, sourceSet ]
