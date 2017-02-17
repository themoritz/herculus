module Herculus.Column where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.EditBox as Edit
import Data.Map (Map)
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv_)
import Lib.Api.Schema.Column (Column(..), columnName)
import Lib.Custom (Id(..))
import Lib.Model.Column (DataType, IsDerived, ReportFormat, ReportLanguage)
import Lib.Model.Table (Table(..))

data Query a
  = Update Input a
  | SetName' String a

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
  Edit.Query String

type Slot =
  Unit

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = cldiv_ "column"
  [ cldiv_ "column__head"
    [ HH.slot unit Edit.comp
              { value: st.input.column ^. columnName
              , placeholder: "Name..."
              , className: "editbox--column-name"
              , show: id
              , validate: Just
              }
              (Just <<< H.action <<< SetName')
    ]
  ]

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  SetName' name next -> do
    H.raise $ SetName name
    pure next
