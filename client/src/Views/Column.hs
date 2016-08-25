module Views.Column where

import React.Flux

import Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text

import Lib.Model.Column
import Lib.Model
import Lib.Types

import Store

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineStatefulView "column" Nothing $ \curText (Entity i col) -> do
  input_
    [ "placeholder" &= ("column name" :: Text)
    , "value" &= fromMaybe (columnName col) curText
    , onChange $ \evt _ -> ([], Just $ Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        let txt = fromMaybe "" curState
        in  if keyCode evt == 13 && not (Text.null txt) -- 13 = Enter
              then (dispatch $ ColumnRename i txt, Just curState)
              else ([], Nothing)
    ]
  button_
    [ onClick $ \_ _ _ -> (dispatch $ TableDeleteColumn i, Nothing)
    ] $ "-"
