module Herculus.DatePicker where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Array (head)
import Data.JSDate (JSDate)
import Flatpickr (flatpickr, setDate, onChange, onClose, destroy, open) as FP
import Flatpickr.Config (defaultConfig) as FP
import Flatpickr.Types (DateType(DateJSDate), Flatpickr) as FP
import Halogen.Query.HalogenM (halt)
import Herculus.Monad (Herc)
import Lib.Custom (ValTime, fromJSDate, toJSDate)

data Query a
  = Initialize a
  | Finalize a
  | Update Input a
  | Open a
  | HandleChange (Array JSDate) (H.SubscribeStatus -> a)
  | HandleClose (H.SubscribeStatus -> a)

type State =
  { input :: Input
  , flatpickr :: Maybe FP.Flatpickr
  }

type Input =
  { date :: ValTime
  }

data Output
  = DateChanged ValTime
  | Closed

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.lifecycleComponent
  { initialState:
      { input: _
      , flatpickr: Nothing
      }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }

  where

  render :: State -> H.ComponentHTML Query
  render _ = HH.input
    [ HP.placeholder "Select date..."
    , HP.type_ HP.InputText
    , HP.ref (H.RefLabel "flatpickr")
    ]

  eval :: Query ~> H.ComponentDSL State Query Output Herc
  eval (Initialize next) = do
    H.getHTMLElementRef (H.RefLabel "flatpickr") >>= case _ of
      Nothing -> halt "DatePicker: Could not find elemet to attach to."
      Just el -> do
        date <- H.gets _.input.date
        flatpickr <- liftEff do
          jsdate <- toJSDate date
          let
            config = FP.defaultConfig
              { defaultDate = FP.DateJSDate jsdate
              }
          FP.flatpickr el config
        modify _
          { flatpickr = Just flatpickr }
        H.subscribe $ H.eventSource
          (\call -> FP.onChange flatpickr \dates _ _ -> call dates)
          (Just <<< H.request <<< HandleChange)
        H.subscribe $ H.eventSource_
          (\call -> FP.onClose flatpickr \dates _ _ -> call)
          (H.request HandleClose)
    pure next

  eval (Finalize next) = do
    mPicker <- H.gets _.flatpickr
    case mPicker of
      Nothing -> halt "DatePicker finalize: not properly initialized."
      Just flatpickr ->
        liftEff $ FP.destroy flatpickr
    pure next

  eval (Update input next) = do
    mPicker <- H.gets _.flatpickr
    case mPicker of
      Nothing -> halt "DatePicker update: not properly initialized."
      Just flatpickr ->
        liftEff $ do
          date <- toJSDate input.date
          void $ FP.setDate (FP.DateJSDate date) false flatpickr
    pure next

  eval (Open next) = do
    mPicker <- H.gets _.flatpickr
    for_ mPicker \picker ->
      liftEff $ FP.open picker
    pure next

  eval (HandleChange dates reply) = do
    case head dates of
      Nothing -> pure unit
      Just jsdate -> do
        date <- liftEff $ fromJSDate jsdate
        H.raise $ DateChanged date
    pure $ reply H.Listening

  eval (HandleClose reply) = do
    H.raise Closed
    pure $ reply H.Listening
