{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Action.Column where

import           Control.Lens
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import qualified Lib.Api.Rest              as Api
import           Lib.Model.Auth            (SessionKey)
import           Lib.Model.Column          (Column, DataType, IsDerived,
                                            ReportFormat, ReportLanguage,
                                            columnKind, columnName,
                                            dataColIsDerived, dataColSourceCode,
                                            dataColType, reportColFormat,
                                            reportColLanguage,
                                            reportColTemplate, _ColumnData,
                                            _ColumnReport)
import           Lib.Types                 (Id)
import           React.Flux.Addons.Servant (request)

import           Action                    (Callback, api, session)
import           Action.Column.Types       (Action (..))

data State = State
  { _column            :: Column
  , _tmpDataType       :: Maybe DataType
  , _tmpIsFormula      :: Maybe IsDerived
  , _tmpFormula        :: Maybe Text
  , _visible           :: Bool
  , _tmpReportLanguage :: Maybe (Maybe ReportLanguage)
  , _tmpReportFormat   :: Maybe ReportFormat
  , _tmpReportTemplate :: Maybe Text
  }

makeLenses ''State

runAction :: Callback s
          -> Maybe SessionKey
          -> Id Column
          -> Action
          -> State -> IO State
runAction mkCallback sessionKey columnId action st = case action of
    SetTmpDataType dt ->
      pure $ st & tmpDataType .~ Just dt
    UnsetTmpDataType ->
      pure $ st & tmpDataType .~ Nothing
    SetTmpIsFormula it ->
      pure $ st & tmpIsFormula .~ Just it
    UnsetTmpIsFormula ->
      pure $ st & tmpIsFormula .~ Nothing
    SetTmpFormula s ->
      pure $ st & tmpFormula .~ Just s
    UnsetTmpFormula ->
      pure $ st & tmpFormula .~ Nothing
    SetVisibility b ->
      pure $ st & visible .~ b
    SetTmpReportLang lang ->
      pure $ st & tmpReportLanguage .~ Just lang
    UnsetTmpReportLang ->
      pure $ st & tmpReportLanguage .~ Nothing
    SetTmpReportFormat format ->
      pure $ st & tmpReportFormat .~ Just format
    UnsetTmpReportFormat ->
      pure $ st & tmpReportFormat .~ Nothing
    SetTmpReportTemplate templ ->
      pure $ st & tmpReportTemplate .~ Just templ
    UnsetTmpReportTemplate ->
      pure $ st & tmpReportTemplate .~ Nothing
    Rename n -> do
      request api (Proxy :: Proxy Api.ColumnSetName)
                  (session sessionKey) columnId n $ mkCallback $ const []
      pure $ st & column . columnName .~ n
    DataColUpdate payload@(dt, inpTyp, src) -> do
      request api (Proxy :: Proxy Api.DataColUpdate)
                  (session sessionKey) columnId payload $ mkCallback $ const []
      pure $ st & column . columnKind . _ColumnData
               %~ (dataColType .~ dt)
                . (dataColIsDerived .~ inpTyp)
                . (dataColSourceCode .~ src)

    -- Report column

    ReportColUpdate payload@(templ, format, lang) -> do
      request api (Proxy :: Proxy Api.ReportColUpdate)
                  (session sessionKey) columnId payload $ mkCallback $ const []
      pure $ st & column . columnKind . _ColumnReport
               %~ (reportColLanguage .~ lang)
                . (reportColFormat .~ format)
                . (reportColTemplate .~ templ)
