module Views.Column where

import React.Flux

import Lib.Model.Column
import Lib.Types

column_ :: Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView Column
column = defineView "column" $ \col -> do
  elemText $ columnName col
