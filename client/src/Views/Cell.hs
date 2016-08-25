module Views.Cell where

import Data.Maybe
import Data.Monoid
import Data.Text (Text, unpack)

import Text.Read (readMaybe)

import React.Flux

import Lib.Model.Column
import Lib.Model.Cell
import Lib.Model.Types
import Lib.Types

import Store

type CellCallback a = a -> StatefulViewEventHandler (Maybe Value)
type CellEventHandler = StatefulViewEventHandler (Maybe Value)

data CellProps = CellProps
  { cpColId :: !(Id Column)
  , cpRecId :: !(Id Record)
  , cpColumn :: !Column
  , cpContent :: !CellContent
  }

cell_ :: CellProps -> ReactElementM eh ()
cell_ !props = view cell props mempty

cell :: ReactView CellProps
cell = defineStatefulView "cell" Nothing $ \mTmpVal props ->
  case cpContent props of
    CellError msg -> elemText $ "Error: " <> msg
    CellValue val -> do
      let col = cpColumn props
          c = cpColId props
          r = cpRecId props
      value_ (columnInputType col)
             (columnDataType col)
             (fromMaybe val mTmpVal)
             (\v _ -> ([], Just $ Just v))
      button_
        [ onClick $ \_ _ -> \case
            Nothing -> ([], Nothing)
            Just tmpVal -> (dispatch $ CellSetValue c r tmpVal, Nothing)
        ] $ "Set"

--

value_ :: InputType -> DataType -> Value -> CellCallback Value
       -> ReactElementM CellEventHandler ()
value_ !inpType !datType !val !cb = case datType of
  DataBool ->
    let (VBool b) = val
    in cellBool_ inpType b (cb . VBool)
  DataString ->
    let (VString s) = val
    in cellString_ inpType s (cb . VString)
  DataNumber ->
    let (VNumber n) = val
    in cellNumber_ inpType n (cb . VNumber)
  DataTime ->
    let (VTime t) = val
    in cellTime_ inpType t (cb . VTime)
  DataRecord t ->
    let (VRecord r) = val
    in cellRecord_ inpType r t (cb . VRecord)
  DataList t ->
    let (VList vs) = val
    in cellList_ inpType t vs (cb . VList)
  DataMaybe t ->
    let (VMaybe v) = val
    in cellMaybe_ inpType t v (cb . VMaybe)

cellBool_ :: InputType -> Bool -> CellCallback Bool
          -> ReactElementM CellEventHandler ()
cellBool_ !inpType !b !cb = case inpType of
  ColumnInput ->
    input_
      [ "type" $= "checkbox"
      , "checked" @= b
      , onChange $ \_ -> cb (not b)
      ]
  ColumnDerived ->
    elemText $ if b then "True" else "False"

cellString_ :: InputType -> Text -> CellCallback Text
            -> ReactElementM CellEventHandler ()
cellString_ !inpType !s !cb = case inpType of
  ColumnInput ->
    input_
      [ "value" &= s
      , onChange $ \evt -> cb (target evt "value")
      ]
  ColumnDerived ->
    elemText s

cellNumber_ :: InputType -> Number -> CellCallback Number
            -> ReactElementM CellEventHandler ()
cellNumber_ !inpType !n !cb = case inpType of
  ColumnInput -> do
    let parseNumber s = Number <$> (readMaybe $ unpack s)
    input_
      [ "value" &= show n
      , onChange $ \evt -> case parseNumber $ target evt "value" of
          Nothing -> const ([], Nothing)
          Just n -> cb n
      ]
  ColumnDerived ->
    elemString $ show n

cellTime_ :: InputType -> Time -> CellCallback Time
          -> ReactElementM CellEventHandler ()
cellTime_ !inpType !t !cb = case inpType of
  ColumnInput ->
    input_
      [ "value" &= formatTime "%F" t
      , onChange $ \evt -> case parseTime "%F" $ target evt "value" of
          Nothing -> const ([], Nothing)
          Just t -> cb t
      ]
  ColumnDerived ->
    elemString $ show t

cellRecord_ :: InputType -> Id Record -> Id Table -> CellCallback (Id Record)
            -> ReactElementM CellEventHandler ()
cellRecord_ !inpType !r !t !cb = case inpType of
  ColumnInput -> undefined
  ColumnDerived -> undefined

cellList_ :: InputType -> DataType -> [Value] -> CellCallback [Value]
          -> ReactElementM CellEventHandler ()
cellList_ !inpType !datType !vs !cb = case inpType of
  ColumnInput -> undefined
  ColumnDerived -> undefined

cellMaybe_ :: InputType -> DataType -> Maybe Value -> CellCallback (Maybe Value)
           -> ReactElementM CellEventHandler ()
cellMaybe_ !inpType !datType !mVal !cb = case inpType of
  ColumnInput -> case mVal of
    Nothing -> do
      let CellValue new = defaultContent datType
      button_
        [ onClick $ \_ _ -> cb (Just new)
        ] "Add"
    Just val -> do
      button_
        [ onClick $ \_ _ -> cb Nothing
        ] "Del"
      value_ inpType datType val (cb . Just)
  ColumnDerived -> case mVal of
    Nothing -> "Nothing"
    Just val -> value_ inpType datType val (cb . Just)
