module Herculus.Ace where

import Herculus.Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ace.Types (Editor)
import Halogen.Query.HalogenM (halt)
import Herculus.Monad (Herc)

data Query a
  = Initialize a
  | Finalize a
  | SetText String a
  | HandleChange (H.SubscribeStatus -> a)

type State =
  { initialValue :: String
  , editor :: Maybe Editor
  }

type Input = String

data Output
  = TextChanged String

ace :: H.Component HH.HTML Query Input Output Herc
ace = H.lifecycleComponent
  { initialState:
      { initialValue: _
      , editor: Nothing
      }
  , render
  , eval
  , receiver: Just <<< H.action <<< SetText
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }

  where

  render :: State -> H.ComponentHTML Query
  render _ = HH.div [ HP.ref (H.RefLabel "ace") ] []

  eval :: Query ~> H.ComponentDSL State Query Output Herc
  eval (Initialize next) = do
    H.getHTMLElementRef (H.RefLabel "ace") >>= case _ of
      Nothing -> pure unit
      Just el -> do
        editor <- liftEff $ Ace.editNode el Ace.ace
        session <- liftEff $ Editor.getSession editor
        modify _
          { editor = Just editor
          }
        value <- gets _.initialValue
        liftEff $ Editor.setValue value (Just (-1)) editor
        H.subscribe $ H.eventSource_
          (Session.onChange session)
          (H.request HandleChange)
    pure next

  eval (Finalize next) = do
    modify _
      { editor = Nothing
      }
    pure next

  eval (SetText value next) = do
    mEditor <- gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        current <- H.liftEff $ Editor.getValue editor
        when (value /= current) do
          void $ H.liftEff $ Editor.setValue value (Just (-1)) editor
    pure next

  eval (HandleChange reply) = do
    mEditor <- gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        value <- liftEff $ Editor.getValue editor
        H.raise $ TextChanged value
    pure (reply H.Listening)
