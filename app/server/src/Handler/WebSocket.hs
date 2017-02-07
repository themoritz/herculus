{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.WebSocket where

import           Auth.Permission
import qualified Lib.Api.Schema.Auth as Api
import           Lib.Api.WebSocket
import           Monads

import qualified ConnectionManager   as Mgr
import           Handler.Rest

handleClientMessage :: MonadHexl m => Mgr.ConnectionId -> WsUpMessage -> m ()
handleClientMessage connection = \case

  WsUpAuthenticate sKey -> do
    response <- handleAuthGetUserInfo (Just sKey)
    case response of
      Api.GetUserInfoSuccess (Api.UserInfo userId _ _ _) ->
        withConnectionMgr $ Mgr.authUser connection userId
      Api.GetUserInfoFailed _ -> pure ()
    sendWS [connection] $ WsDownAuthResponse response

  WsUpLogout ->
    withConnectionMgr $ Mgr.forgetUser connection

  WsUpSubscribe projectId -> do
    withConnectionMgr (Mgr.getUser connection) >>= \case
      Nothing -> sendWS [connection] $
        WsDownSubscribeError "You are not authenticated."
      Just userId -> permissionProject' userId projectId >>= \case
        False -> sendWS [connection] $
          WsDownSubscribeError "You don't have access to this project."
        True -> withConnectionMgr $ Mgr.subscribeProject connection projectId

  WsUpUnsubscribe ->
    withConnectionMgr $ Mgr.unsubscribe connection
