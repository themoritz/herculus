{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

type Arg t a = Behavior t (Either String a)
type Res t m a = Event t () -> m (Event t (ReqResult a))

data RestApi t m = MonadWidget t m => RestApi
  { projectCreate :: Arg t Text -> Res t m (Id Project)
  , projectList   :: Res t m [(Id Project, Text)]
  , tableCreate   :: Arg t TableCreate -> Res t m (Id Table)
  , tableList     :: Arg t (Id Project) -> Res t m [(Id Table, Text)]
  , tableData     :: Arg t (Id Table) -> Res t m [(Id Record, [(Id Column, Text)])]
  }

api :: forall t m. MonadWidget t m => RestApi t m
api =
  let (project :<|> table :<|> column :<|> record :<|> cell) =
            client (Proxy :: Proxy Routes)
                   (Proxy :: Proxy m)
                   (constDyn (BasePath "/"))
      (projectC :<|> projectL) = project
      (tableC :<|> tableL :<|> tableD) = table
  in RestApi
       { projectCreate = projectC
       , projectList   = projectL
       , tableCreate   = tableC
       , tableList     = tableL
       , tableData     = tableD
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
