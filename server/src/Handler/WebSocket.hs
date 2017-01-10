{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.WebSocket where

import           Auth.Permission
import           Lib.Api.WebSocket
import           Lib.Model.Auth
import           Lib.Model.Class
import           Monads

import qualified ConnectionManager as Mgr
import           Handler.Rest

handleClientMessage :: MonadHexl m => Mgr.ConnectionId -> WsUpMessage -> m ()
handleClientMessage connection = \case

  WsUpAuthenticate sKey -> do
    response <- handleAuthGetUserInfo sKey
    case response of
      GetUserInfoSuccess (UserInfo userId _ _) ->
        withConnectionMgr $ Mgr.authUser connection userId
      GetUserInfoFailed _ -> pure ()
    sendWS [connection] $ WsDownAuthResponse response

  WsUpLogout ->
    withConnectionMgr $ Mgr.forgetUser connection

  WsUpSubscribe projectId -> do
    let i = fromClientId projectId
    withConnectionMgr (Mgr.getUser connection) >>= \case
      Nothing -> sendWS [connection] $
        WsDownSubscribeError "You are not authenticated."
      Just userId -> permissionProject' userId i >>= \case
        False -> sendWS [connection] $
          WsDownSubscribeError "You don't have access to this project."
        True -> withConnectionMgr $ Mgr.subscribeProject connection i

  WsUpUnsubscribe ->
    withConnectionMgr $ Mgr.unsubscribe connection
