{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module WebSocket
  ( JSWebSocket
  , jsonWebSocketNew
  , jsonWebSocketSend
  ) where

import           Prelude                           hiding (all, concat,
                                                    concatMap, div, mapM, mapM_,
                                                    sequence, span)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad                     (void)
import           Control.Monad.Reader

import           Data.Aeson                        hiding (String)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Lazy              (toStrict)
import           Data.Functor                      (($>))
import           Data.IORef
import           Data.Text                         (Text)
import           Data.Text.Encoding

import           GHCJS.Buffer
import           GHCJS.DOM.EventM                  (on)
import           GHCJS.DOM.MessageEvent
import           GHCJS.DOM.Types                   hiding (Text)
import           GHCJS.DOM.WebSocket               (closeEvent, message, open)
import qualified GHCJS.DOM.WebSocket               as GD
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal.Pure                (PFromJSVal (..))
import           GHCJS.Types
import           JavaScript.TypedArray.ArrayBuffer as JS

import           React.Flux                        (SomeStoreAction,
                                                    executeAction)

data JSWebSocket = JSWebSocket
  { webSocketRef   :: IORef (Maybe WebSocket)
  , webSocketQueue :: TQueue ByteString
  }

instance Show JSWebSocket where
  show _ = "WebSocket"

jsonWebSocketNew :: FromJSON a
                 => Text
                 -- ^ Url
                 -> (a -> IO [SomeStoreAction])
                 -- ^ Actions to perform after message was received
                 -> IO [SomeStoreAction]
                 -- ^ After a new connection was opened
                 -> IO [SomeStoreAction]
                 -- ^ After the connection was closed
                 -> IO JSWebSocket
jsonWebSocketNew url msgHandler openHandler closeHandler = do
  ref <- newIORef Nothing
  isOpen <- atomically $ newTMVar ()
  queue <- atomically newTQueue
  let onMessage msg = case decodeStrict msg of
        Nothing -> pure ()
        Just a  -> msgHandler a >>= mapM_ executeAction
      onOpen = do
        _ <- atomically $ tryPutTMVar isOpen ()
        openHandler >>= mapM_ executeAction
      onClose = do
        _ <- atomically $ tryTakeTMVar isOpen
        writeIORef ref Nothing
        closeHandler >>= mapM_ executeAction
        void $ forkIO $ do
          threadDelay 1000000 -- 1 sec
          start
      start = do
        ws <- newWebSocket url onMessage onOpen onClose
        writeIORef ref (Just ws)
  -- Open initial connection
  liftIO start
  -- Send messages when something is in the queue
  _ <- forkIO $ forever $ do
    msg <- atomically $ do
      msg <- readTQueue queue
      _ <- readTMVar isOpen
      pure msg
    success <- readIORef ref >>= \case
      Nothing -> pure False
      Just ws -> catch (webSocketSend ws msg $> True)
                       (\(_ :: SomeException) -> pure False)
    unless success $ atomically $ unGetTQueue queue msg
  pure JSWebSocket
    { webSocketRef = ref
    , webSocketQueue = queue
    }

jsonWebSocketSend :: ToJSON a => a -> JSWebSocket -> IO ()
jsonWebSocketSend a (JSWebSocket _ queue) =
  atomically $ writeTQueue queue $ toStrict $ encode a

--------------------------------------------------------------------------------

newWebSocket :: Text
             -> (ByteString -> IO ()) -> IO () -> IO ()
             -> IO WebSocket
newWebSocket url onMessage onOpen onClose = do
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
  pure ws

webSocketSend :: WebSocket -> ByteString -> IO ()
webSocketSend ws bs = do
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
