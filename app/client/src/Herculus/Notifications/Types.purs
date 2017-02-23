module Herculus.Notifications.Types where

import Herculus.Prelude

data Kind
  = Info
  | Success
  | Warn
  | Error

type Config =
  { kind :: Kind
  , message :: String
  , detail :: Maybe String
  }
