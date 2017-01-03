{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Action.RowCache where

import           Control.DeepSeq  (NFData)
import           GHC.Generics     (Generic)
import           Lib.Model        (Entity)
import           Lib.Model.Cell   (CellContent)
import           Lib.Model.Column (Column)
import           Lib.Model.Row    (Row)
import           Lib.Types        (Id)

data Action
  = Add (Id Row) [(Entity Column, CellContent)]
  | Delete (Id Row)
  | Set [(Id Row, [(Entity Column, CellContent)])]
  deriving (NFData, Generic)
