module Main where

import Herculus.Prelude
import Halogen as H
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef)
import Data.Nullable (toMaybe)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import Control.Monad.Eff.Exception (error)
import DOM.HTML.Document (body)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Herculus.App (app)
import Herculus.Monad (runHerc, HercEffects)

main :: String -> String -> Boolean -> Eff HercEffects Unit
main apiUrl webSocketUrl hotReload = do
  tokenRef <- newRef Nothing
  runHalogenAff $ do
    body <- if hotReload
      then do
        mBody <- map toMaybe $ liftEff $ window >>= document >>= body
        case mBody of
          Just b -> pure b
          Nothing -> throwError (error "Body not found.")
      else
        awaitBody
    let
      wiring =
        { apiUrl
        , webSocketUrl
        , authTokenRef: tokenRef
        }
    runUI (H.hoist (runHerc wiring) app) unit body
