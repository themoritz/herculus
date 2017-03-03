module Herculus.EditBox where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.Event.KeyboardEvent (code) as DOM
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv, cldiv_, focusElement)

data Query v a
  = Update (Input v) a
  | StartEdit a
  | SetText String a
  | TrySave a
  | CancelEdit a

type Input v =
  { value :: v
  , placeholder :: String
  , className :: String
  , show :: v -> String
  , validate :: String -> Maybe v
  }

type State v =
  { input :: Input v
  , tmpValue :: Maybe v
  , invalidText :: Maybe String
  , editing :: Boolean
  }

ref :: H.RefLabel
ref = H.RefLabel "input"

comp :: forall v. H.Component HH.HTML (Query v) (Input v) v Herc
comp = H.component
  { initialState:
    { input: _
    , tmpValue: Nothing
    , invalidText: Nothing
    , editing: false
    }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  }

render :: forall v. State v -> H.ComponentHTML (Query v)
render st = cldiv_ st.input.className
  [ case st.editing of
      false -> cldiv ("editbox " <> if text == "" then "gray" else "")
        [ HE.onClick (HE.input_ StartEdit)
        ]
        [ HH.text (if text == "" then st.input.placeholder else text) ]
      true -> HH.input
        [ HP.placeholder st.input.placeholder
        , HP.ref ref
        , HP.classes
          [ H.ClassName "editbox__input"
          , H.ClassName (if isJust st.invalidText then "editbox__input--invalid" else "")
          ]
        , HP.value text
        , HE.onValueInput (HE.input SetText)
        , HE.onKeyDown \e -> case DOM.code e of
            "Enter"  -> Just (H.action TrySave)
            "Escape" -> Just (H.action CancelEdit)
            _        -> Nothing
        , HE.onBlur (HE.input_ CancelEdit)
        ]
   ]

   where

   text = fromMaybe (st.input.show st.input.value) $
          st.input.show <$> st.tmpValue
      <|> st.invalidText

eval :: forall v. Query v ~> H.ComponentDSL (State v) (Query v) v Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  StartEdit next -> do
    modify \st -> st
      { editing = true
      , tmpValue = Just st.input.value
      }
    focusElement ref
    pure next

  SetText str next -> do
    validate <- H.gets _.input.validate
    case validate str of
      Nothing -> modify _
        { tmpValue = Nothing
        , invalidText = Just str
        }
      Just v -> modify _
        { tmpValue = Just v
        , invalidText = Nothing
        }
    pure next
    
  TrySave next -> do
    gets _.tmpValue >>= case _ of
      Nothing -> reset
      Just v -> do
        reset
        H.raise v
    pure next

  CancelEdit next -> do
    reset
    pure next

  where

  reset = modify _
    { tmpValue = Nothing
    , invalidText = Nothing
    , editing = false
    }
