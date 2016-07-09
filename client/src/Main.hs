{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Data.Aeson
import Data.List.NonEmpty (toList)
import Data.Text (pack, Text)
import Data.Proxy

import Data.ByteString.Lazy (toStrict)

import Reflex.Dom

import Servant.API
import Servant.Reflex

import Lib
import Lib.Api.WebSocket
import Lib.Api.Rest

type TriggerAndResult t m a = Event t () -> m (Event t (ReqResult a))

data RestApi t m = MonadWidget t m => RestApi
  { projectCreate :: Behavior t (Either String Text)
                  -> TriggerAndResult t m (Id Project)
  , projectList   :: TriggerAndResult t m [(Id Project, Text)]
  }

api :: forall t m. MonadWidget t m => RestApi t m
api =
  let (project :<|> table :<|> column :<|> record :<|> cell) =
            client (Proxy :: Proxy Routes)
                   (Proxy :: Proxy m)
                   (constDyn (BasePath "/"))
      (projectC :<|> projectL) = project
  in RestApi
       { projectCreate = projectC
       , projectList = projectL
       }

main :: IO ()
main = mainWidget widget

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
  projects

projects :: MonadWidget t m => m ()
projects = el "div" $ do
  inp <- textInput def
  let projectName = (Right . pack) <$> current (_textInput_value inp)
  create <- button "Create"
  newProj <- projectCreate api projectName create
  dynText =<< holdDyn "" (fmap show $ fmapMaybe reqSuccess newProj)
  dynText =<< holdDyn "" (fmapMaybe reqFailure newProj)

ws :: MonadWidget t m => Event t [WsUpMessage] -> m (Event t WsDownMessage)
ws messages = do
  ws' <- webSocket "ws://localhost:3000/websocket" $
    def & webSocketConfig_send .~ (fmap.fmap) (toStrict . encode) messages
  pure $ fmapMaybe decodeStrict $ _webSocket_recv ws'
