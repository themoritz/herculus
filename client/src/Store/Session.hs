{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}

module Store.Session where

import           Data.Aeson         (FromJSON, ToJSON)
import qualified Data.Aeson         as Json
import           GHCJS.Marshal.Pure (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Nullable     (Nullable, nullableToMaybe)
import           GHCJS.Types        (JSString, JSVal)

import           Lib.Model.Auth     (SessionKey)

key :: JSString
key = "sessionKey"

recoverSession :: IO (Maybe SessionKey)
recoverSession = do
  mValue <- recoverLS key
  pure $ do
    value <- mValue
    Json.decode value

persistSession :: SessionKey -> IO ()
persistSession sKey = persistLS key $ Json.encode sKey

recoverLS :: PFromJSVal a => JSString -> IO (Maybe a)
recoverLS key = fmap pFromJSVal . nullableToMaybe <$> basilGet key

persistLS :: PToJSVal a => JSString -> a -> IO ()
persistLS key value = basilSet key $ pToJSVal value

foreign import javascript unsafe "basil.set($1, $2)"
  basilSet :: JSString -> JSVal -> IO ()

foreign import javascript unsafe "basil.get($1)"
  basilGet :: JSString -> IO (Nullable JSVal)
