{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Column
  ( column
  ) where

import Control.Monad (void)

import Data.Text (unpack)
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
       -> Dynamic t Column -> m (Event t Column, Event t ())
column tableId columnId dynColumn = el "div" $ do

  initial <- sample $ current dynColumn
  let set = updated dynColumn

  let columnIdArg = constant $ Right columnId

  name <- textInputT def
            { _textInputConfig_setValue = unpack . columnName <$> set
            , _textInputConfig_initialValue = unpack . columnName $ initial
            }
  nameSet <- button "Set"
  loader' (Api.columnSetName api columnIdArg (Right <$> current name)) nameSet

  dt <- dropdown (columnDataType initial)
                 (constDyn dataTypeEntries)
                 (def :: DropdownConfig t DataType)
                   { _dropdownConfig_setValue =
                       (columnDataType <$> set)
                   }
  setDt <- button "Set"
  let dataType = _dropdown_value dt
  loader' (Api.columnSetDataType api columnIdArg (Right <$> current dataType))
          setDt

  it <- dropdown (columnInputType initial)
                 (constDyn inputTypeEntries) $
                 (def :: DropdownConfig t InputType)
                   { _dropdownConfig_setValue =
                         (columnInputType <$> set)
                   }
  let inputType = _dropdown_value it
  trigger <- button "Set"

  sourceAttr <- forDyn dynColumn $ \col -> case columnInputType col of
    ColumnInput   -> "style" =: "display: none"
    ColumnDerived -> "style" =: "display: inherit"

  rec let inputArg = do
            inputType' <- current inputType
            source' <- current source
            pure $ Right (inputType', source')

      loader' (Api.columnSetInput api columnIdArg inputArg) sourceSet

      compiledD <- holdDyn (columnCompileResult initial) $ columnCompileResult <$> set

      (source, sourceSet) <- elDynAttr "div" sourceAttr $ do
        val <- textInputT (def :: TextInputConfig t)
                 { _textInputConfig_setValue = unpack . columnSourceCode <$> set
                 , _textInputConfig_initialValue = unpack . columnSourceCode $ initial
                 , _textInputConfig_attributes = constDyn ("style" =: "width: 160px")
                 }

        void $ dynWidget compiledD $ \case
          CompileResultCode _        -> text "Ok"
          CompileResultNone      -> pure ()
          CompileResultError msg -> text $ "Error: " <> unpack msg

        pure (val, trigger)

  let columnB = Column <$> pure tableId
                       <*> current name
                       <*> current dataType
                       <*> current inputType
                       <*> current source
                       <*> current compiledD

  delete <- button "Delete"

  pure (tag columnB $ leftmost [ nameSet, setDt, sourceSet ], delete)
