{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson

import Data.ByteString.Lazy (toStrict)

import Reflex.Dom

import Lib

main :: IO ()
main = mainWidget widget

widget :: MonadWidget t m => m ()
widget = el "div" $ do
  greet <- button "Greet"
  messages <- ws $ WsUpGreet "Foo" <$ greet
  messagesDyn <- holdDyn (WsDownGreet "") messages
  messagesDynStr <- mapDyn show messagesDyn
  dynText messagesDynStr

ws :: MonadWidget t m => Event t WsUpMessage -> m (Event t WsDownMessage)
ws messages = do
  ws' <- webSocket "ws://localhost:3000/websocket" $ def & webSocketConfig_send .~ ((:[]) . toStrict . encode <$> messages)
  pure $ fmapMaybe decodeStrict $ _webSocket_recv ws'
