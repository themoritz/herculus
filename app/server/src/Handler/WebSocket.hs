{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.WebSocket where

import           Lib.Prelude

import qualified Lib.Api.Schema.Auth as Api
import           Lib.Api.WebSocket

import           Auth.Permission
import qualified ConnectionManager   as Mgr
import           Handler.Rest
import           Monads

handleClientMessage
  :: (MonadHexl m, MonadIO m)
  => Mgr.ConnectionId -> WsUpMessage -> m ()
handleClientMessage connection = \case

  WsUpSubscribe sKey projectId -> do
    let
      stop = sendWS [connection] . WsDownSubscribeError
    response <- handleAuthGetUserInfo (Just sKey)
    case response of
      Api.GetUserInfoFailed msg -> stop ("Authentication failed: " <> msg)
      Api.GetUserInfoSuccess (Api.UserInfo userId _ _ _) -> do
        withConnectionMgr $ Mgr.authUser connection userId
        permissionProject' userId projectId >>= \case
          False -> stop "You don't have access to this project."
          True -> withConnectionMgr $ Mgr.subscribeProject connection projectId
