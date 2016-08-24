module Views.Record where

import React.Flux

import Lib.Model.Types
import Lib.Types

record_ :: Record -> ReactElementM eh ()
record_ !c = view record c mempty

record :: ReactView Record
record = defineView "record" $ \r -> do
  "record"
