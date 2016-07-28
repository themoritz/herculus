module Widgets.Record
  ( record
  ) where

import Reflex.Dom
import Lib.Types
import Lib.Model.Types

record :: MonadWidget t m
       => Id Record -> m ()
record recId = pure ()
