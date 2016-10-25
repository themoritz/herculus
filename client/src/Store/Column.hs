{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Store.Column
  ( module Action.Column
  , module Store.Column
  ) where

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
import           Action.Column             (Action (..))

data State = State
  { _stColumn            :: Column
  , _stTmpDataType       :: Maybe DataType
  , _stTmpIsFormula      :: Maybe IsDerived
  , _stTmpFormula        :: Maybe Text
  , _stVisible           :: Bool
  , _stTmpReportLanguage :: Maybe (Maybe ReportLanguage)
  , _stTmpReportFormat   :: Maybe ReportFormat
  , _stTmpReportTemplate :: Maybe Text
  }

makeLenses ''State

mkState :: Column -> State
mkState c = State
  { _stColumn = c
  , _stTmpDataType = Nothing
  , _stTmpIsFormula = Nothing
  , _stTmpFormula = Nothing
  , _stVisible = False
  , _stTmpReportLanguage = Nothing
  , _stTmpReportFormat = Nothing
  , _stTmpReportTemplate = Nothing
  }

runAction :: Callback
          -> Maybe SessionKey
          -> Id Column
          -> Action
          -> State -> IO State
runAction mkCallback sessionKey columnId action st = case action of
    SetTmpDataType dt ->
      pure $ st & stTmpDataType .~ Just dt
    UnsetTmpDataType ->
      pure $ st & stTmpDataType .~ Nothing
    SetTmpIsFormula it ->
      pure $ st & stTmpIsFormula .~ Just it
    UnsetTmpIsFormula ->
      pure $ st & stTmpIsFormula .~ Nothing
    SetTmpFormula s ->
      pure $ st & stTmpFormula .~ Just s
    UnsetTmpFormula ->
      pure $ st & stTmpFormula .~ Nothing
    SetVisibility b ->
      pure $ st & stVisible .~ b
    SetTmpReportLang lang ->
      pure $ st & stTmpReportLanguage .~ Just lang
    UnsetTmpReportLang ->
      pure $ st & stTmpReportLanguage .~ Nothing
    SetTmpReportFormat format ->
      pure $ st & stTmpReportFormat .~ Just format
    UnsetTmpReportFormat ->
      pure $ st & stTmpReportFormat .~ Nothing
    SetTmpReportTemplate templ ->
      pure $ st & stTmpReportTemplate .~ Just templ
    UnsetTmpReportTemplate ->
      pure $ st & stTmpReportTemplate .~ Nothing
    Rename n -> do
      request api (Proxy :: Proxy Api.ColumnSetName)
                  (session sessionKey) columnId n $ mkCallback $ const []
      pure $ st & stColumn . columnName .~ n
    DataColUpdate payload@(dt, inpTyp, src) -> do
      request api (Proxy :: Proxy Api.DataColUpdate)
                  (session sessionKey) columnId payload $ mkCallback $ const []
      pure $ st & stColumn . columnKind . _ColumnData
               %~ (dataColType .~ dt)
                . (dataColIsDerived .~ inpTyp)
                . (dataColSourceCode .~ src)

    -- Report column

    ReportColUpdate payload@(templ, format, lang) -> do
      request api (Proxy :: Proxy Api.ReportColUpdate)
                  (session sessionKey) columnId payload $ mkCallback $ const []
      pure $ st & stColumn . columnKind . _ColumnReport
               %~ (reportColLanguage .~ lang)
                . (reportColFormat .~ format)
                . (reportColTemplate .~ templ)
