{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.WebSocket where

import           Control.Monad    (void)

import           Data.Maybe       (mapMaybe)

import           Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo

import           Lib.Api.WebSocket
import           Monads

handleClientMessage :: MonadHexl m => WsUpMessage -> m ()
handleClientMessage wsUp = pure ()
