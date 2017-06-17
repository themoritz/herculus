module Herculus.Config.Common.Header where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Herculus.EditBox as Edit
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1)
import Herculus.Config.Common (section)
import Herculus.Monad (Herc)
import Herculus.Utils (clbutton_, cldiv_, faIcon_)

data Query a
  = Update Input a
  | SetName' String a
  | Close' a
  | Reset' a
  | Delete' a
  | Save' a

type Input =
  { name :: String
  , unsaved :: Boolean
  , subTitle :: forall p i. HH.HTML p i
  }

type State =
  { input :: Input
  }

type Child =
  Edit.Query String <\/>
  Const Void

type Slot =
  Unit \/
  Unit

data Output
  = SetName String
  | Close
  | Delete
  | Reset
  | Save

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st =
  let
    head = cldiv_ "flex"
      [ cldiv_ "flex-auto"
        [ HH.slot' cp1 unit Edit.comp
                 { value: st.input.name
                 , placeholder: "Name..."
                 , className: "bold editbox"
                 , inputClassName: "editbox__input"
                 , invalidClassName: "editbox__input--invalid"
                 , show: id
                 , validate: Just
                 , clickable: true
                 }
                 case _ of
                   Edit.Save v _ -> Just $ H.action $ SetName' v
                   Edit.Cancel -> Nothing
        ]
      , cldiv_ ""
        [ clbutton_ "button--pure" Close'
          [ faIcon_ "close fa-lg fa-fw" ]
        ]
      ]
    unsaved = st.input.unsaved
    disabled = if unsaved then "" else " button--disabled"
    body = cldiv_ "flex items-end"
      [ cldiv_ "font-smaller flex-auto"
        [ st.input.subTitle
        ]
      , cldiv_ "font-smaller"
        [ clbutton_ "button bold mr1" Delete'
          [ faIcon_ "close red mr1"
          , HH.text "Delete"
          ]
        , HH.button
          [ HP.class_ (H.ClassName $ "button bold mr1" <> disabled)
          , HE.onClick (if unsaved then HE.input_ Reset' else const Nothing)
          , HP.disabled (not unsaved)
          ]
          [ faIcon_ "undo gray mr1"
          , HH.text "Reset"
          ]
        , clbutton_ "button bold" Save'
          [ faIcon_ $ "check mr1 " <> if unsaved then "green" else "gray"
          , HH.text "Save"
          ]
        ]
      ]
  in
    section "wrench" head body 

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  SetName' n next -> do
    H.raise (SetName n)
    pure next

  Close' next -> do
    H.raise Close
    pure next

  Save' next -> do
    H.raise Save
    pure next

  Delete' next -> do
    H.raise Delete
    pure next

  Reset' next -> do
    H.raise Reset
    pure next
