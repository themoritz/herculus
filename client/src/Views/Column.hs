module Views.Column where

import React.Flux

import Lib.Model.Column
import Lib.Model
import Lib.Types

import Store

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineView "column" $ \(Entity i col) -> do
  elemText $ columnName col
  button_
    [ onClick $ \_ _ -> dispatch $ TableDeleteColumn i
    ] $ "-"
