{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( Routes
    , api
    ) where

import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           Servant
import           Types           (Subscriber, SubscriberAddResult)

type Routes = "subscriber" :> "add" :> ReqBody '[PlainText] Text :> Post '[JSON] SubscriberAddResult
         :<|> "subscriber" :> "all" :> Get '[JSON] [UTCTime]

api :: Proxy Routes
api = Proxy
