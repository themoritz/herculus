{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Store.RecordCache
  ( module Action.RecordCache
  , runAction
  , recordCache
  , State
  ) where

import           Control.Arrow             (second)
import           Control.Lens
import           Control.Monad             (when)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Proxy                (Proxy (..))
import qualified Lib.Api.Rest              as Api
import           Lib.Model                 (Entity (Entity))
import           Lib.Model.Auth            (SessionKey)
import           Lib.Model.Cell            (CellContent)
import           Lib.Model.Column          (Column)
import           Lib.Model.Record          (Record)
import           Lib.Model.Table           (Table)
import           Lib.Types                 (Id)
import           React.Flux.Addons.Servant (request)

import           Action                    (Action (RecordCacheAction),
                                            Callback, api, session)
import           Action.RecordCache        (Action (..))

data State = State
  { _recordCache :: Map (Id Record) (Map (Id Column) (Column, CellContent)) }

makeLenses ''State

runAction :: Callback
          -> Maybe SessionKey
          -> Id Table
          -> Action.RecordCache.Action
          -> State -> IO State
runAction mkCallback sessionKey tableId = \case
    Add recordId record ->
      let toCol (Entity c col, content) = (c, (col, content))
          recMap = Map.fromList $ map toCol record
      in  \cache -> pure $ cache & recordCache . at recordId .~ Just recMap

    Delete r ->
      \cache -> pure $ cache & recordCache . at r .~ Nothing

    Get -> \cache -> do
      when ( Map.null $ cache ^. recordCache) $
        request api (Proxy :: Proxy Api.RecordListWithData)
                    (session sessionKey) tableId $ mkCallback $
                    \records -> [RecordCacheAction tableId $ Set records]
      pure cache

    Set recs ->
      let toCol (Entity c col, content) = (c, (col, content))
          recMaps = map (second $ Map.fromList . map toCol) recs
      in  \cache -> pure $ cache & recordCache .~ Map.fromList recMaps
