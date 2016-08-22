module Api.Rest where

import Data.Proxy

import React.Flux.Addons.Servant

import Servant.API

import Lib.Types
import Lib.Model.Types
import Lib.Api.Rest

data RestApi = RestApi
  { projectCreate :: Project -> HandleResponse (Id Project) -> IO ()
  }

apiCfg :: ApiRequestConfig Routes
apiCfg = ApiRequestConfig "/" 0

api :: RestApi
api =
  let (project :<|> _) = request apiCfg (Proxy :: Proxy Routes)
      (projectC :<|> _) = project
  in RestApi
       { projectCreate = projectC
       }
