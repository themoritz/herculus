module Widgets.Record
  ( record
  ) where

import Reflex.Dom
import Lib

record :: MonadWidget t m
       => Id Record -> m ()
record recId = el "div" $
  text $ take 5 $ show recId
