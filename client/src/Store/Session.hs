{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

module Store.Session where

import qualified Data.Aeson           as Json
import           Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text.Encoding   as Text
import           GHCJS.Marshal.Pure   (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Nullable       (Nullable, nullableToMaybe)
import           GHCJS.Types          (JSString, JSVal)

import           Lib.Model.Auth       (SessionKey)

sessionKey :: JSString
sessionKey = "sessionKey"

recoverSession :: IO (Maybe SessionKey)
recoverSession = do
  mValue <- recoverLS sessionKey
  pure $ do
    value <- mValue
    Json.decode $ fromStrict $ Text.encodeUtf8 value

persistSession :: SessionKey -> IO ()
persistSession = persistLS sessionKey . Text.decodeUtf8 . toStrict . Json.encode

recoverLS :: PFromJSVal a => JSString -> IO (Maybe a)
recoverLS key = fmap pFromJSVal . nullableToMaybe <$> basilGet key

persistLS :: PToJSVal a => JSString -> a -> IO ()
persistLS key value = basilSet key $ pToJSVal value

foreign import javascript unsafe "basil.set($1, $2)"
  basilSet :: JSString -> JSVal -> IO ()

foreign import javascript unsafe "basil.get($1)"
  basilGet :: JSString -> IO (Nullable JSVal)
