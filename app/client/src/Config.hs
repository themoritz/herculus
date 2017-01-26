module Config
  ( webSocketUrl
  , apiUrl
  ) where

import           Data.Text          (Text)

import           Data.JSString.Text (textFromJSString)
import           GHCJS.Types        (JSString)

foreign import javascript unsafe
  "hexl$getWebSocketUrl()"
  js_webSocketUrl :: JSString


webSocketUrl :: Text
webSocketUrl = textFromJSString js_webSocketUrl

foreign import javascript unsafe
  "hexl$getApiUrl()"
  js_apiUrl :: JSString

apiUrl :: JSString
apiUrl = js_apiUrl
