{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Action.RecordCache where

import           Control.DeepSeq  (NFData)
import           GHC.Generics     (Generic)
import           Lib.Model        (Entity)
import           Lib.Model.Cell   (CellContent)
import           Lib.Model.Column (Column)
import           Lib.Model.Record (Record)
import           Lib.Types        (Id)

data Action
  = Add (Id Record) [(Entity Column, CellContent)]
  | Delete (Id Record)
  | Set [(Id Record, [(Entity Column, CellContent)])]
  deriving (NFData, Generic)
