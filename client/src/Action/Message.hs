{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Action.Message where

import           Control.DeepSeq (NFData)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

data Action
  = SetInfo Text
  | SetSuccess Text
  | SetWarning Text
  | SetError Text
  | Unset
  deriving (NFData, Generic)
