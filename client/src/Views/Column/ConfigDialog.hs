{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Views.Column.ConfigDialog where

import           Control.DeepSeq  (NFData)
import           Control.Lens
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Lib.Model.Column (Column, DataType, IsDerived, ReportFormat,
                                   ReportLanguage)
import           Lib.Types        (Id)
import           React.Flux       (ReactStore, StoreData (..), mkStore)

newtype State = State { unState :: Map (Id Column) DialogState }

data DialogState = DialogState
  { _stTmpDataType       :: Maybe DataType
  , _stTmpIsFormula      :: Maybe IsDerived
  , _stTmpFormula        :: Maybe Text
  , _stVisible           :: Bool
  , _stTmpReportLanguage :: Maybe (Maybe ReportLanguage)
  , _stTmpReportFormat   :: Maybe ReportFormat
  , _stTmpReportTemplate :: Maybe Text
  }

makeLenses ''DialogState

store :: ReactStore State
store = mkStore $ State Map.empty

data Action = Action (Id Column) DialogAction

data DialogAction
  = SetTmpDataType         DataType
  | UnsetTmpDataType
  | SetTmpIsFormula        IsDerived
  | UnsetTmpIsFormula
  | SetTmpFormula          Text
  | UnsetTmpFormula
  | SetVisibility          Bool
  | SetTmpReportLang       (Maybe ReportLanguage)
  | UnsetTmpReportLang
  | SetTmpReportFormat     ReportFormat
  | UnsetTmpReportFormat
  | SetTmpReportTemplate   Text
  | UnsetTmpReportTemplate
  deriving (NFData, Generic)

instance StoreData State where
  type StoreAction State = Action
  transform (Action i action) (State st) = pure $ State $
    case action of
      SetTmpDataType dt ->
        st & at i . _Just . stTmpDataType .~ Just dt
      UnsetTmpDataType ->
        st & at i . _Just . stTmpDataType .~ Nothing
      SetTmpIsFormula it ->
        st & at i . _Just . stTmpIsFormula .~ Just it
      UnsetTmpIsFormula ->
        st & at i . _Just . stTmpIsFormula .~ Nothing
      SetTmpFormula s ->
        st & at i . _Just . stTmpFormula .~ Just s
      UnsetTmpFormula ->
        st & at i . _Just . stTmpFormula .~ Nothing
      SetVisibility b ->
        st & at i . _Just . stVisible .~ b
      SetTmpReportLang lang ->
        st & at i . _Just . stTmpReportLanguage .~ Just lang
      UnsetTmpReportLang ->
        st & at i . _Just . stTmpReportLanguage .~ Nothing
      SetTmpReportFormat format ->
        st & at i . _Just . stTmpReportFormat .~ Just format
      UnsetTmpReportFormat ->
        st & at i . _Just . stTmpReportFormat .~ Nothing
      SetTmpReportTemplate templ ->
        st & at i . _Just . stTmpReportTemplate .~ Just templ
      UnsetTmpReportTemplate ->
        st & at i . _Just . stTmpReportTemplate .~ Nothing
