{-# LANGUAGE FlexibleContexts #-}
-- |

module Store.Types where

import           Control.DeepSeq           (NFData)

import qualified Config
import           React.Flux.Addons.Servant (ApiRequestConfig (..),
                                            HandleResponse,
                                            RequestTimeout (NoTimeout))

import           Lib.Api.Rest              as Api

class Monad m => MonadStore m where
  apiCall :: NFData a => (HandleResponse a -> IO ()) -> m a


api :: ApiRequestConfig Routes
api = ApiRequestConfig Config.apiUrl NoTimeout
