module Views.Record where

import React.Flux

import Lib.Model.Types
import Lib.Model

import Store
import Views.Combinators

record_ :: Entity Record -> ReactElementM eh ()
record_ !c = view record c mempty

record :: ReactView (Entity Record)
record = defineView "record" $ \(Entity i _) -> do
  faButton_ "minus-circle" $ dispatch $ TableDeleteRecord i
