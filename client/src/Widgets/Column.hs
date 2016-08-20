{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Column
  ( column
  ) where

import Control.Monad (void)

import Data.Text (Text, pack)
import Data.Monoid
import Data.Maybe (fromMaybe)

import Reflex.Dom

import Api.Rest (loader, loader', api)
import qualified Api.Rest as Api

import Data.Map (Map)
import qualified Data.Map as Map

import Lib.Types
import Lib.Model
import Lib.Model.Types
import Lib.Model.Column

import Misc

inputTypeEntries :: Map InputType Text
inputTypeEntries = Map.fromList
  [ (ColumnInput, "Input")
  , (ColumnDerived, "Derived")
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

  dataType <- dataTypeDropdown (columnDataType <$> dynColumn)
  setDt <- button "Set"
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
        val <- _textArea_value <$> textArea (def :: TextAreaConfig t)
                 { _textAreaConfig_setValue = columnSourceCode <$> set
                 , _textAreaConfig_initialValue = columnSourceCode $ initial
                 }

        void $ dynWidget compiledD $ \case
          CompileResultCode _    -> text "Ok"
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

--

data Branch
  = BBool
  | BString
  | BNumber
  | BTime
  | BRecord
  | BList
  | BMaybe
  deriving (Eq, Ord)

toBranch :: DataType -> Branch
toBranch = \case
  DataBool     -> BBool
  DataString   -> BString
  DataNumber   -> BNumber
  DataTime     -> BTime
  DataRecord _ -> BRecord
  DataList _   -> BList
  DataMaybe _  -> BMaybe

branchEntries :: Map Branch Text
branchEntries = Map.fromList
 [ (BBool, "Bool")
 , (BString, "String")
 , (BNumber, "Number")
 , (BTime, "Time")
 , (BRecord, "Record")
 , (BList, "List")
 , (BMaybe, "Maybe")
 ]

subType :: DataType -> DataType
subType = \case
  DataList t   -> t
  DataMaybe t  -> t
  _            -> DataNumber

subTable :: DataType -> Id Table
subTable (DataRecord t) = t
subTable _              = nullObjectId

dataTypeDropdown :: forall t m. MonadWidget t m
                 => Dynamic t DataType -> m (Dynamic t DataType)
dataTypeDropdown input = do
  initial <- sample $ current input
  dd <- dropdown (toBranch initial)
                 (constDyn branchEntries)
                 (def :: DropdownConfig t Branch)
                   { _dropdownConfig_setValue = toBranch <$> updated input
                   }
  let thisD = _dropdown_value dd
  subE <- dyn $ ffor thisD $ \case
    BMaybe  -> (fmap.fmap) Just $ dataTypeDropdown (subType <$> input)
    BList   -> (fmap.fmap) Just $ dataTypeDropdown (subType <$> input)
    _       -> pure $ constDyn Nothing
  tblE <- dyn $ ffor thisD $ \case
    BRecord -> (fmap.fmap) Just $ tableDropdown (subTable <$> input)
    _       -> pure $ constDyn Nothing
  subD <- switcherDyn (constDyn Nothing) subE
  tblD <- switcherDyn (constDyn Nothing) tblE
  let go this sub tbl = case (this, sub, tbl) of
        (BBool, _, _)        -> DataBool
        (BString, _, _)      -> DataString
        (BNumber, _, _)      -> DataNumber
        (BTime, _, _)        -> DataTime
        (BRecord, _, Just t) -> DataRecord t
        (BList, s, _)        -> DataList $ fromMaybe DataNumber s
        (BMaybe, s, _)       -> DataMaybe $ fromMaybe DataNumber s
  pure (go <$> thisD <*> subD <*> tblD)

tableDropdown :: MonadWidget t m => Dynamic t (Id Table) -> m (Dynamic t (Id Table))
tableDropdown input = do
  initial <- sample $ current input
  postBuild <- getPostBuild
  tables <- loader (Api.tableListGlobal api) postBuild
  tablesDyn <- holdDyn [] tables
  let tableMap = Map.fromList . map (\(Entity i (Table _ name)) -> (i, name)) <$> tablesDyn
  dd <- dropdown initial
                 tableMap
                 def { _dropdownConfig_setValue = updated input
                     }
  pure $ _dropdown_value dd
