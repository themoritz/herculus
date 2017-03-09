module Herculus.Project.Settings where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Modal as Modal
import DOM.Event.KeyboardEvent (code)
import Data.String (length)
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv_, faIcon_, focusElement)

data Query a
  = Update Input a
  | StartEditName a
  | CancelEditName a
  | SetName String a
  | SaveName' a
  | Delete' a
  | ReallyDelete a

type Input =
  { name :: String
  }

data Output
  = SaveName String
  | Delete

type State =
  { input :: Input
  , tmpName :: Maybe String
  }

projectNameRef :: H.RefLabel
projectNameRef = H.RefLabel "projectNameRef" 

deleteConfirmInput :: Modal.Input Boolean
deleteConfirmInput =
  { title: "Delete Project"
  , text: "Do you really want to delete this project? This cannot be undone \
          \at the moment."
  , actions:
    [ { icon: "close red", label: "Cancel", value: false }
    , { icon: "check green", label: "Delete", value: true }
    ]
  }

type Child =
  Modal.Query Boolean

type Slot =
  Unit

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    , tmpName: Nothing
    }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HH.div_
  [ cldiv_ "project-name" case st.tmpName of
    Nothing ->
      [ HH.span_
        [ HH.text st.input.name ]
      , HH.text " "
      , HH.button
        [ HP.class_ (H.ClassName "button--pure button--on-dark gray ml1 align-middle")
        , HP.title "Change project name"
        , HE.onClick (HE.input_ StartEditName)
        ]
        [ faIcon_ "pencil" ]
      , HH.button
        [ HP.class_ (H.ClassName "button--pure button--on-dark gray ml1 align-middle")
        , HP.title "Delete project"
        , HE.onClick (HE.input_ Delete')
        ]
        [ faIcon_ "times" ]
      ]
    Just name ->
      [ HH.input
        [ HP.value name
        , HP.class_ (H.ClassName "header-input")
        , HP.ref projectNameRef
        , HE.onValueInput (HE.input SetName)
        , HE.onKeyDown \e -> case code e of
            "Enter"  -> Just (H.action SaveName')
            "Escape" -> Just (H.action CancelEditName)
            _        -> Nothing
        , HE.onBlur (HE.input_ CancelEditName)
        ]
      ]
  , HH.slot unit Modal.comp deleteConfirmInput $ case _ of
      true  -> Just $ H.action ReallyDelete
      false -> Nothing
  ]


eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  StartEditName next -> do
    n <- H.gets _.input.name
    modify _{ tmpName = Just n }
    focusElement projectNameRef
    pure next

  CancelEditName next -> do
    modify _{ tmpName = Nothing }
    pure next

  SetName name next -> do
    modify _{ tmpName = Just name }
    pure next

  SaveName' next -> do
    { tmpName } <- get
    for_ tmpName \name -> when (length name > 0) $ H.raise $ SaveName name
    pure next

  Delete' next -> do
    H.query unit $ H.action Modal.Open
    pure next

  ReallyDelete next -> do
    H.raise Delete
    pure next
