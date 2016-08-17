{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens

import Data.Aeson
import Data.ByteString.Lazy (toStrict)

import Reflex.Dom

import Lib.Api.WebSocket

import Misc
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

      tSelect <- divClass "two columns" $ do
        divClass "container" $ do
          pList <- divClass "row" $
            divClass "twelve columns" $ projectList never
          divClass "row" $
            divClass "twelve columns" $ do
              currentProj <- holdDyn Nothing $ Just <$> (_projectList_selectProject pList)
              switchEventWith _tableList_selectTable $ dynWidget currentProj $ \case
                Nothing -> pure $ TableList never
                Just projId -> tableList projId never

      divClass "ten columns" $ do
        table tSelect cellChanges columnChanges

ws :: MonadWidget t m => Event t [WsUpMessage] -> m (Event t WsDownMessage)
ws messages = do
  ws' <- webSocket "ws://localhost:3000/websocket" $
    def & webSocketConfig_send .~ (fmap.fmap) (toStrict . encode) messages
  pure $ fmapMaybe decodeStrict $ _webSocket_recv ws'
