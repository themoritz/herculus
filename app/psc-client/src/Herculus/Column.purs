module Herculus.Column where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Map (Map)
import Herculus.Monad (Herc)
import Lib.Api.Schema.Column (Column(..))
import Lib.Custom (Id(..))
import Lib.Model.Column (DataType, IsDerived, ReportFormat, ReportLanguage)
import Lib.Model.Table (Table(..))

data Query a
  = Update Input a

type Input =
  { column :: Column
  , tables :: Map (Id Table) String
  }

data Output
  = SetName String
  | Delete
  | SaveReportCol String ReportFormat (Maybe ReportLanguage)
  | SaveDataCol DataType IsDerived String

type State =
  { input :: Input
  }

type Child =
  Const Void

type Slot =
  Void

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    }
  , receiver: const Nothing
  , render
  , eval
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HH.text "Column"

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next
