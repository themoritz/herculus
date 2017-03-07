module Herculus.EditBox where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.Event.KeyboardEvent (code) as DOM
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv, focusElement)

data SaveKey
  = Enter
  | Tab

data Query v a
  = Update (Input v) a
  | StartEdit (Maybe String) a
  | SetText String a
  | TrySave SaveKey a
  | CancelEdit a

type Input v =
  { value :: v
  , placeholder :: String
  , className :: String
  , inputClassName :: String
  , invalidClassName :: String
  , show :: v -> String
  , validate :: String -> Maybe v
  , clickable :: Boolean
  }

type State v =
  { input :: Input v
  , tmpValue :: Maybe v
  , invalidText :: Maybe String
  , editing :: Boolean
  }

data Output v
  = Save v SaveKey
  | Cancel

ref :: H.RefLabel
ref = H.RefLabel "input"

comp :: forall v. H.Component HH.HTML (Query v) (Input v) (Output v) Herc
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
render st = HH.div_
  [ case st.editing of
      false -> cldiv (st.input.className <> if text == "" then " gray" else "")
        ( if st.input.clickable
          then [ HE.onClick (HE.input_ $ StartEdit Nothing) ]
          else [ ]
        )
        [ HH.text (if text == "" then st.input.placeholder else text) ]
      true -> HH.input
        [ HP.placeholder st.input.placeholder
        , HP.ref ref
        , HP.classes
          [ H.ClassName st.input.inputClassName
          , H.ClassName (if isJust st.invalidText then st.input.invalidClassName else "")
          ]
        , HP.value text
        , HE.onValueInput (HE.input SetText)
        , HE.onKeyDown \e -> case DOM.code e of
            "Enter"  -> Just (H.action $ TrySave Enter)
            "Tab"    -> Just (H.action $ TrySave Tab)
            "Escape" -> Just (H.action CancelEdit)
            _        -> Nothing
        , HE.onBlur (HE.input_ CancelEdit)
        ]
   ]

   where

   text = fromMaybe (st.input.show st.input.value) $
          st.input.show <$> st.tmpValue
      <|> st.invalidText

eval :: forall v. Query v ~> H.ComponentDSL (State v) (Query v) (Output v) Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  StartEdit mChar next -> do
    modify \st -> st
      { editing = true
      , tmpValue = Just st.input.value
      }
    for_ mChar setText
    focusElement ref
    pure next

  SetText str next -> do
    setText str
    pure next
    
  TrySave key next -> do
    gets _.tmpValue >>= case _ of
      Nothing -> reset
      Just v -> do
        reset
        H.raise $ Save v key
    pure next

  CancelEdit next -> do
    reset
    H.raise Cancel
    pure next

  where

  setText str = do
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

  reset = modify _
    { tmpValue = Nothing
    , invalidText = Nothing
    , editing = false
    }
