module Herculus.Ace where

import Herculus.Prelude
import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ace (ACE)
import Ace.Types (Editor, Range, EditSession)
import Halogen.Query.HalogenM (halt)
import Herculus.Monad (Herc)

data Query a
  = Initialize a
  | Update Input a
  | Finalize a
  | HandleChange (H.SubscribeStatus -> a)

type State =
  { input :: Input
  , markers :: Array Int
  , editor :: Maybe Editor
  }

data LintType
  = LintError
  | LintWarn
  | LintInfo

type LintAnnotation =
  { msg :: String
  , range :: Range
  , kind :: LintType
  }

type Input =
  { value :: String
  , linter :: Array LintAnnotation
  , mode :: String
  }

data Output
  = TextChanged String

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.lifecycleComponent
  { initialState:
      { input: _
      , markers: []
      , editor: Nothing
      }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }

  where

  render :: State -> H.ComponentHTML Query
  render _ = HH.div [ HP.ref (H.RefLabel "ace") ] []

  eval :: Query ~> H.ComponentDSL State Query Output Herc
  eval (Initialize next) = do
    H.getHTMLElementRef (H.RefLabel "ace") >>= case _ of
      Nothing -> halt "Ace: Could not find elemet to attach to."
      Just el -> do
        editor <- liftEff $ Ace.editNode el Ace.ace
        session <- liftEff $ Editor.getSession editor
        modify _
          { editor = Just editor
          }
        { input } <- H.get
        liftEff do
          setBlockScrollingInfinity editor
          Editor.setValue input.value (Just (-1)) editor
          Session.setMode input.mode session
          Session.setUseSoftTabs true session
          Session.setTabSize 2 session
          Editor.setTheme "ace/theme/github" editor
          Editor.setMaxLines 100 editor
          Editor.setHighlightActiveLine false editor
        -- Update annotations
        setAnnotations input.linter session
        -- Subscribe to changes
        H.subscribe $ H.eventSource_
          (Session.onChange session)
          (H.request HandleChange)
    pure next

  eval (Finalize next) = do
    modify _
      { editor = Nothing
      }
    pure next

  eval (Update input next) = do
    mEditor <- H.gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        session <- liftEff $ Editor.getSession editor
        liftEff do
          -- Update value
          current <- Editor.getValue editor
          when (input.value /= current) do
            void $ Editor.setValue input.value (Just (-1)) editor
          -- Update mode
          Ace.TextMode currentMode <- Session.getMode session
          when (input.mode /= currentMode) do
            void $ Session.setMode input.mode session
        -- Update annotations
        setAnnotations input.linter session
    pure next

  eval (HandleChange reply) = do
    mEditor <- H.gets _.editor
    case mEditor of
      Nothing -> halt "Ace not properly initialized."
      Just editor -> do
        value <- liftEff $ Editor.getValue editor
        H.raise $ TextChanged value
    pure (reply H.Listening)

setAnnotations
  :: Array LintAnnotation -> EditSession
  -> H.ComponentDSL State Query Output Herc Unit
setAnnotations annos session = do
  { markers } <- get
  markers' <- liftEff do
    for markers \i -> Session.removeMarker i session
    for annos \anno -> do
      Session.addMarker anno.range "linter-error" "string" false session
  modify _{ markers = markers' }
  pure unit

foreign import setBlockScrollingInfinity
  :: forall eff. Editor -> Eff (ace :: ACE | eff) Unit
