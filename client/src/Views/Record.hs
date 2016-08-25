module Views.Record where

import React.Flux

import Lib.Model.Types
import Lib.Model
import Lib.Types

import Store

record_ :: Entity Record -> ReactElementM eh ()
record_ !c = view record c mempty

record :: ReactView (Entity Record)
record = defineView "record" $ \(Entity i _) -> do
  button_
    [ onClick $ \_ _ -> dispatch $ TableDeleteRecord i
    ] "-"
