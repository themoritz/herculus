module Views.Cell where

import Data.Maybe
import Data.Monoid
import Data.Text (Text)

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
  where

    value_ :: InputType -> DataType -> Value -> CellCallback Value
           -> ReactElementM CellEventHandler ()
    value_ !inpType !datType !val !cb =
      case datType of
        DataBool ->
          let (VBool b) = val
          in cellBool_ inpType b (cb . VBool)
        DataString ->
          let (VString s) = val
          in cellString_ inpType s (cb . VString)

    cellBool_ :: InputType -> Bool -> CellCallback Bool -> ReactElementM CellEventHandler ()
    cellBool_ !inpType !b !cb =
      case inpType of
        ColumnInput -> undefined

    cellString_ :: InputType -> Text -> CellCallback Text -> ReactElementM CellEventHandler ()
    cellString_ !inpType !s !cb =
      case inpType of
        ColumnInput ->
          input_
            [ "value" &= s
            , onChange $ \evt -> cb (target evt "value")
            ]
        ColumnDerived -> elemText s
