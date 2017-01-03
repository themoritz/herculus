{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

module WebSocket where

import           Prelude                           hiding (all, concat,
                                                    concatMap, div, mapM, mapM_,
                                                    sequence, span)

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import           Data.Text                         (Text)
import           Data.Text.Encoding
import           GHCJS.Buffer
import           GHCJS.DOM.EventM                  (on)
import           GHCJS.DOM.MessageEvent
import           GHCJS.DOM.Types                   hiding (Text)
import           GHCJS.DOM.WebSocket               (closeEvent, message, open)
import qualified GHCJS.DOM.WebSocket               as GD
import           GHCJS.Foreign.Internal
import           GHCJS.Types
import           JavaScript.TypedArray.ArrayBuffer as JS

import           Data.Aeson                        hiding (String)
import           Data.ByteString.Lazy              (toStrict)
import           Data.Foldable

import           React.Flux                        (SomeStoreAction,
                                                    executeAction)
import           React.Flux.Addons.Free

data JSWebSocket = JSWebSocket { unWebSocket :: WebSocket }

jsonWebSocketNew :: FromJSON a => Text
                               -> (a -> IO [SomeStoreAction])
                               -> FreeFlux s JSWebSocket
jsonWebSocketNew url handler = do
  let onMessage msg = case decodeStrict msg of
        Nothing -> pure ()
        Just a -> do
          actions <- handler a
          for_ actions executeAction
  newWebSocket url onMessage (pure ()) (pure ())

jsonWebSocketSend :: ToJSON a => a -> JSWebSocket -> FreeFlux s ()
jsonWebSocketSend a ws = webSocketSend ws $ toStrict $ encode a

--

newWebSocket :: Text
             -> (ByteString -> IO ()) -> IO () -> IO ()
             -> FreeFlux s JSWebSocket
newWebSocket url onMessage onOpen onClose = liftIO $ do
  ws <- GD.newWebSocket url (Just [] :: Maybe [Text])
  _ <- on ws open $ liftIO onOpen
  GD.setBinaryType ws ("arraybuffer" :: String)
  _ <- on ws message $ do
    e <- ask
    d <- getData e
    liftIO $ case jsTypeOf d of
      String -> onMessage $ encodeUtf8 $ pFromJSVal d
      _ -> do
        ab <- unsafeFreeze $ pFromJSVal d
        onMessage $ toByteString 0 Nothing $ createFromArrayBuffer ab
  _ <- on ws closeEvent $ liftIO onClose
  return $ JSWebSocket ws

webSocketSend :: JSWebSocket -> ByteString -> FreeFlux s ()
webSocketSend (JSWebSocket ws) bs = liftIO $ do
  let (b, off, len) = fromByteString bs
  -- TODO: remove this logic when https://github.com/ghcjs/ghcjs-base/issues/49
  -- is fixed.
  ab <- ArrayBuffer <$> if BS.length bs == 0
    then jsval . getArrayBuffer <$> create 0
    else return $ js_dataView off len $ jsval $ getArrayBuffer b
  GD.send ws $ Just ab

foreign import javascript safe
  "new DataView($3,$1,$2)"
  js_dataView :: Int -> Int -> JSVal -> JSVal
