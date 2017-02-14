module Herculus.DatePicker where

import Herculus.Prelude
import Flatpickr (flatpickr, setDate, onChange, destroy) as FP
import Flatpickr.Config (defaultConfig) as FP
import Flatpickr.Types (DateType(DateJSDate), Flatpickr) as FP
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Array (head)
import Data.JSDate (JSDate)
import Halogen.Query.HalogenM (halt)
import Herculus.Monad (Herc)

data Query a
  = Initialize a
  | Finalize a
  | SetDate JSDate a
  | HandleChange (Array JSDate) (H.SubscribeStatus -> a)

type State =
  { initialDate :: JSDate
  , flatpickr :: Maybe FP.Flatpickr
  }

type Input = JSDate

data Output
  = DateChanged JSDate

datePicker :: H.Component HH.HTML Query Input Output Herc
datePicker = H.lifecycleComponent
  { initialState:
      { initialDate: _
      , flatpickr: Nothing
      }
  , render
  , eval
  , receiver: Just <<< H.action <<< SetDate
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }

  where

  render :: State -> H.ComponentHTML Query
  render _ = HH.div [ HP.ref (H.RefLabel "flatpickr") ] []

  eval :: Query ~> H.ComponentDSL State Query Output Herc
  eval (Initialize next) = do
    H.getHTMLElementRef (H.RefLabel "flatpickr") >>= case _ of
      Nothing -> halt "DatePicker: Could not find elemet to attach to."
      Just el -> do
        date <- gets _.initialDate
        let
          config = FP.defaultConfig
            { defaultDate = FP.DateJSDate date
            }
        flatpickr <- liftEff $ FP.flatpickr el config
        modify _
          { flatpickr = Just flatpickr }
        H.subscribe $ H.eventSource
          (\call -> FP.onChange flatpickr \dates _ _ -> call dates)
          (Just <<< H.request <<< HandleChange)
    pure next

  eval (Finalize next) = do
    mPicker <- gets _.flatpickr
    case mPicker of
      Nothing -> halt "DatePicker not properly initialized."
      Just flatpickr ->
        liftEff $ FP.destroy flatpickr
    pure next

  eval(SetDate date next) = do
    mPicker <- gets _.flatpickr
    case mPicker of
      Nothing -> halt "DatePicker not properly initialized."
      Just flatpickr ->
        liftEff $ FP.setDate (FP.DateJSDate date) false flatpickr
    pure next

  eval(HandleChange dates reply) = do
    case head dates of
      Nothing -> pure unit
      Just date -> H.raise $ DateChanged date
    pure $ reply H.Listening