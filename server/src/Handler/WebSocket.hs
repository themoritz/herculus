{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.WebSocket where

import           Lib.Api.WebSocket
import           Monads

handleClientMessage :: MonadHexl m => WsUpMessage -> m ()
handleClientMessage _ = pure ()
