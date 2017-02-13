module Main where

import Herculus.Prelude
import Control.Monad.Aff.Bus as Bus
import Halogen as H
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (newRef)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Window (document)
import Data.Nullable (toMaybe)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Herculus.App as App
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
    bus <- Bus.make
    let
      wiring =
        { apiUrl
        , webSocketUrl
        , authTokenRef: tokenRef
        , notificationBus: bus
        }
    io <- runUI (H.hoist (runHerc wiring) App.app) unit body
    forever do
      cfg <- Bus.read bus
      io.query $ H.action $ App.Notify cfg
