module Views.Column where

import React.Flux

import           Data.Text (Text)
import qualified Data.Text as Text

import Lib.Model.Column
import Lib.Model
import Lib.Types

import Store

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineStatefulView "column" "" $ \curText (Entity i col) -> do
  input_
    [ "placeholder" &= ("column name" :: Text)
    , "value" &= columnName col
    , onChange $ \evt _ -> ([], Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        if keyCode evt == 13 && not (Text.null curState) -- 13 = Enter
           then (dispatch $ ColumnRename i curState, Just curState)
           else ([], Nothing)
    ]
--   button_
--     [ onClick $ \_ _ -> (dispatch $ TableDeleteColumn i, Nothing)
--     ] $ "-"
