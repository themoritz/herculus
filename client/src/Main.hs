{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens

import Data.Aeson
import Data.ByteString.Lazy (toStrict)

import Reflex.Dom

import Lib.Api.WebSocket

import Widgets.ProjectList
import Widgets.TableList
import Widgets.Table

main :: IO ()
main = mainWidget $ do
  wsDown <- ws never

  let cellChanges = flip fmapMaybe wsDown $ \case
        WsDownCellsChanged entries -> Just entries
        _                          -> Nothing
      columnChanges = flip fmapMaybe wsDown $ \case
        WsDownColumnsChanged entries -> Just entries
        _                            -> Nothing

  divClass "container" $ do
    divClass "row" $ do

      tList <- divClass "two columns" $ do
        divClass "container" $ do
          pList <- divClass "row" $
            divClass "twelve columns" $ projectList never
          divClass "row" $
            divClass "twelve columns" $
              tableList never $ _projectList_selectProject pList

      divClass "ten columns" $ do
        table (_tableList_selectTable tList) cellChanges columnChanges

ws :: MonadWidget t m => Event t [WsUpMessage] -> m (Event t WsDownMessage)
ws messages = do
  ws' <- webSocket "ws://localhost:3000/websocket" $
    def & webSocketConfig_send .~ (fmap.fmap) (toStrict . encode) messages
  pure $ fmapMaybe decodeStrict $ _webSocket_recv ws'
