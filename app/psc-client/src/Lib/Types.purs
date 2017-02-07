module Lib.Types where

import Data.Generic (class Generic)

newtype Id a = Id String

derive instance genericId :: Generic (Id a)

newtype Ref a = Ref { unRef :: String }

derive instance genericRef :: Generic (Ref a)

-- Id tags

data ColumnTag
data ProjectTag
