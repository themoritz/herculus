{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens

import Data.Aeson
import Data.List.NonEmpty (toList)
import Data.Text (pack)

import Data.ByteString.Lazy (toStrict)

import Reflex.Dom

import Lib
import Lib.Api.WebSocket

import Widgets.ProjectList
import Widgets.TableList
import Widgets.Table

main :: IO ()
main = mainWidget $ do
  elClass "div" "container" $ do
    elClass "div" "row" $ do
      elClass "div" "three columns" $ do
        elClass "div" "container" $ do
          pList <- elClass "div" "row" $
            elClass "div" "twelve columns" $ projectList def
          elClass "div" "row" $
            elClass "div" "twelve columns" $
              tableList $ def & tableListConfig_loadProject .~ (pList ^. projectList_selectProject)
      elClass "div" "nine columns" table

widget :: MonadWidget t m => m ()
widget = el "div" $ do
  inp <- textInput def
  greet <- button "Greet"
  store <- button "Store"
  listAll <- button "List"
  let currentInp = current $ _textInput_value inp
  messages <- ws $ toList <$> mergeList
    [ attachWith (\v _ -> WsUpGreet $ pack v) currentInp greet
    , attachWith (\v _ -> WsUpStore $ pack v) currentInp store
    , WsUpList <$ listAll
    ]
  messagesDyn <- holdDyn (WsDownGreet "") messages
  messagesDynStr <- mapDyn show messagesDyn
  dynText messagesDynStr

ws :: MonadWidget t m => Event t [WsUpMessage] -> m (Event t WsDownMessage)
ws messages = do
  ws' <- webSocket "ws://localhost:3000/websocket" $
    def & webSocketConfig_send .~ (fmap.fmap) (toStrict . encode) messages
  pure $ fmapMaybe decodeStrict $ _webSocket_recv ws'
