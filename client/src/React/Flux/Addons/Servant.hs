{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module React.Flux.Addons.Servant(
    HandleResponse
  , RequestTimeout(..)
  , ApiRequestConfig(..)
  , request
  , HasAjaxRequest(..)
  , Request(..)
) where

import           Data.Aeson
import           Data.JSString      (JSString)
import qualified Data.JSString      as JSS
import qualified Data.JSString.Text as JSS
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Data.Typeable      (Proxy (..))
import           GHC.TypeLits
import           GHCJS.Marshal      (FromJSVal (..), toJSVal_aeson)
import           GHCJS.Types        (JSVal, nullRef)
import           React.Flux
import           React.Flux.Ajax
import           Servant.API

data Request = Request {
    segments :: [JSString]
  , rHeaders :: [(JSString, JSString)]
  , rQuery   :: [(JSString, JSString)]
  , rBody    :: IO JSVal
  , rTimeout :: RequestTimeout
}

type HandleResponse a = Either (Int,String) a -> IO [SomeStoreAction]

data ApiRequestConfig api = ApiRequestConfig
    { urlPrefix :: JSString
    , timeout   :: RequestTimeout
    }

request :: HasAjaxRequest endpoint => ApiRequestConfig api -> Proxy endpoint -> MkRequest endpoint
request (ApiRequestConfig p t) endpoint = toRequest endpoint (Request [p] [] [] (pure nullRef) t)

class HasAjaxRequest endpoint where
    type MkRequest endpoint
    toRequest :: Proxy endpoint -> Request -> MkRequest endpoint

instance (HasAjaxRequest a, HasAjaxRequest b) => HasAjaxRequest (a :<|> b) where
    type MkRequest (a :<|> b) = MkRequest a :<|> MkRequest b
    toRequest Proxy r =
        toRequest (Proxy :: Proxy a) r :<|>
        toRequest (Proxy :: Proxy b) r

instance (ToJSON a, HasAjaxRequest sub) => HasAjaxRequest (ReqBody '[JSON] a :> sub) where
    type MkRequest (ReqBody '[JSON] a :> sub) = a -> MkRequest sub
    toRequest _ r body = toRequest (Proxy :: Proxy sub) (r
        { rBody = toJSVal_aeson body >>= js_JSONstringify
        , rHeaders = rHeaders r ++ [("Content-Type", "application/json")]
        })

instance (KnownSymbol sym, HasAjaxRequest sub) => HasAjaxRequest (sym :> sub) where
    type MkRequest (sym :> sub) = MkRequest sub
    toRequest _ r = toRequest (Proxy :: Proxy sub) (r { segments = segments r ++ [seg]})
        where
            seg = JSS.pack $ symbolVal (Proxy :: Proxy sym)

instance (ToHttpApiData v, HasAjaxRequest sub) => HasAjaxRequest (Capture sym v :> sub) where
    type MkRequest (Capture sym v :> sub) = v -> MkRequest sub
    toRequest _ r v = toRequest (Proxy :: Proxy sub) (r { segments = segments r ++ [v'] })
        where
            v' = JSS.pack $ T.unpack $ toUrlPiece v

instance (KnownSymbol sym, ToHttpApiData a, HasAjaxRequest sub) => HasAjaxRequest (Header sym a :> sub) where
    type MkRequest (Header sym a :> sub) = Maybe a -> MkRequest sub
    toRequest _ r Nothing  = toRequest (Proxy :: Proxy sub) r
    toRequest _ r (Just a) = toRequest (Proxy :: Proxy sub) (r { rHeaders = rHeaders r ++ [(sym',a')]})
        where
            sym' = JSS.pack $ symbolVal (Proxy :: Proxy sym)
            a' = JSS.pack $ T.unpack $ toUrlPiece a

instance (KnownSymbol sym, ToHttpApiData a, HasAjaxRequest sub) => HasAjaxRequest (QueryParam sym a :> sub) where
    type MkRequest (QueryParam sym a :> sub) = Maybe a -> MkRequest sub
    toRequest _ r Nothing = toRequest (Proxy :: Proxy sub) r
    toRequest _ r (Just a) = toRequest (Proxy :: Proxy sub) r { rQuery = rQuery r ++ [(sym', a')]}
        where
            sym' = JSS.pack $ symbolVal (Proxy :: Proxy sym)
            a' = JSS.pack $ T.unpack $ toUrlPiece a

instance (ReflectMethod m, FromJSON a) => HasAjaxRequest (Verb m s '[JSON] a) where
    type MkRequest (Verb m s '[JSON] a) = HandleResponse a -> IO ()
    toRequest _ r handler = do
        body <- rBody r
        let query :: JSString = case rQuery r of
                        [] -> ""
                        qs -> "?" <> JSS.intercalate "&" (map (\(x,y) -> x <> "=" <> y) qs)
        let req = AjaxRequest
                  { reqMethod = JSS.textToJSString $ T.decodeUtf8 $ reflectMethod (Proxy :: Proxy m)
                  , reqURI = JSS.intercalate "/" (segments r) <> query
                  , reqHeaders = rHeaders r ++ [("Accept", "application/json")]
                  , reqBody = body
                  , reqTimeout = rTimeout r
                  }
        ajax req $ \resp ->
            if respStatus resp == 200
                then do
                    j <- js_JSONParse $ respResponseText resp
                    mv <- fromJSVal j
                    case mv of
                        Nothing -> handler $ Left (500, "Unable to convert response body")
                        Just v -> case fromJSON v of
                            Success v' -> handler $ Right v'
                            Error e -> handler $ Left (500, e)
                else handler $ Left (respStatus resp, JSS.unpack $ respResponseText resp)

foreign import javascript unsafe
    "JSON['parse']($1)"
    js_JSONParse :: JSString -> IO JSVal

foreign import javascript unsafe
    "JSON['stringify']($1)"
    js_JSONstringify :: JSVal -> IO JSVal
