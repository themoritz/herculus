{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Store.Column
  ( module Action.Column
  , module Store.Column
  ) where

import           Control.Lens
import           Data.Proxy                (Proxy (..))
import qualified Lib.Api.Rest              as Api
import           Lib.Model.Auth            (SessionKey)
import           Lib.Model.Column          (Column, columnKind, columnName,
                                            dataColIsDerived, dataColSourceCode,
                                            dataColType, reportColFormat,
                                            reportColLanguage,
                                            reportColTemplate, _ColumnData,
                                            _ColumnReport)
import           Lib.Types                 (Id)
import           React.Flux.Addons.Servant (request)

import           Action                    (Callback, api, session)
import           Action.Column             (Action (..))

type State = Column

runAction :: Callback
          -> SessionKey
          -> Id Column
          -> Action
          -> State -> IO State
runAction mkCallback sessionKey columnId action st =
  case action of
    Rename n -> do
      request api (Proxy :: Proxy Api.ColumnSetName)
                  (session sessionKey) columnId n $ mkCallback $ const []
      pure $ st & columnName .~ n
    DataColUpdate dt inpTyp src -> do
      request api (Proxy :: Proxy Api.DataColUpdate)
                  (session sessionKey) columnId (dt, inpTyp, src) $ mkCallback $ const []
      pure $ st & columnKind . _ColumnData
               %~ (dataColType .~ dt)
                . (dataColIsDerived .~ inpTyp)
                . (dataColSourceCode .~ src)
    ReportColUpdate templ format lang -> do
      request api (Proxy :: Proxy Api.ReportColUpdate)
                  (session sessionKey) columnId (templ, format, lang) $ mkCallback $ const []
      pure $ st & columnKind . _ColumnReport
               %~ (reportColLanguage .~ lang)
                . (reportColFormat .~ format)
                . (reportColTemplate .~ templ)
