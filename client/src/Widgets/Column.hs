{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Column
  ( column
  ) where

import Control.Monad (void)

import Data.Text (Text)
import Data.Monoid

import Reflex.Dom

import Api.Rest (loader', api)
import qualified Api.Rest as Api

import Data.Map (Map)
import qualified Data.Map as Map

import Lib.Types
import Lib.Model.Types
import Lib.Model.Column

import Misc

inputTypeEntries :: Map InputType Text
inputTypeEntries = Map.fromList
  [ (ColumnInput, "Input")
  , (ColumnDerived, "Derived")
  ]

dataTypeEntries :: Map DataType Text
dataTypeEntries = Map.fromList
 [ (DataBool, "Bool")
 , (DataString, "String")
 , (DataNumber, "Number")
 , (DataRecord, "Record")
 ]

column :: forall t m. MonadWidget t m
       => Id Table -> Id Column
       -> Dynamic t Column -> m (Event t Column, Event t ())
column tableId columnId dynColumn = el "div" $ do

  initial <- sample $ current dynColumn
  let set = updated dynColumn

  let columnIdArg = constDyn $ Right columnId

  name <- _textInput_value <$> textInput def
            { _textInputConfig_setValue = columnName <$> set
            , _textInputConfig_initialValue = columnName $ initial
            }
  nameSet <- button "Set"
  loader' (Api.columnSetName api columnIdArg (Right <$> name)) nameSet

  dt <- dropdown (columnDataType initial)
                 (constDyn dataTypeEntries)
                 (def :: DropdownConfig t DataType)
                   { _dropdownConfig_setValue =
                       (columnDataType <$> set)
                   }
  setDt <- button "Set"
  let dataType = _dropdown_value dt
  loader' (Api.columnSetDataType api columnIdArg (Right <$> dataType))
          setDt

  it <- dropdown (columnInputType initial)
                 (constDyn inputTypeEntries) $
                 (def :: DropdownConfig t InputType)
                   { _dropdownConfig_setValue =
                         (columnInputType <$> set)
                   }
  let inputType = _dropdown_value it
  trigger <- button "Set"

  let sourceAttr = ffor dynColumn $ \col -> case columnInputType col of
        ColumnInput   -> "style" =: "display: none"
        ColumnDerived -> "style" =: "display: inherit"

  rec let inputArg = do
            inputType' <- inputType
            source' <- source
            pure $ Right (inputType', source')

      loader' (Api.columnSetInput api columnIdArg inputArg) sourceSet

      compiledD <- holdDyn (columnCompileResult initial) $ columnCompileResult <$> set

      (source, sourceSet) <- elDynAttr "div" sourceAttr $ do
        val <- _textInput_value <$> textInput (def :: TextInputConfig t)
                 { _textInputConfig_setValue = columnSourceCode <$> set
                 , _textInputConfig_initialValue = columnSourceCode $ initial
                 , _textInputConfig_attributes = constDyn ("style" =: "width: 160px")
                 }

        void $ dynWidget compiledD $ \case
          CompileResultCode _        -> text "Ok"
          CompileResultNone      -> pure ()
          CompileResultError msg -> text $ "Error: " <> msg

        pure (val, trigger)

  let columnB = Column <$> pure tableId
                       <*> current name
                       <*> current dataType
                       <*> current inputType
                       <*> current source
                       <*> current compiledD

  delete <- button "Delete"

  pure (tag columnB $ leftmost [ nameSet, setDt, sourceSet ], delete)
