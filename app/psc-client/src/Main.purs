module Main where

import Herculus.Prelude
import Halogen as H
import Herculus.Root as Root
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (makeVar, takeVar)
import Control.Monad.Eff.Exception (error)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Window (document)
import Data.Nullable (toMaybe)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Herculus.Monad (runHerc, HercEffects)
import Herculus.Router (pRoute)
import Routing (matchesAff)

main :: String -> String -> Boolean -> Eff HercEffects Unit
main apiUrl webSocketUrl hotReload = do
  runHalogenAff $ do
    body <- if hotReload
      then do
        mBody <- map toMaybe $ liftEff $ window >>= document >>= body
        case mBody of
          Just b -> pure b
          Nothing -> throwError (error "Body not found.")
      else
        awaitBody
    notifications <- makeVar
    let
      wiring =
        { apiUrl
        , webSocketUrl
        , notifications
        }
    io <- runUI (H.hoist (runHerc wiring) Root.comp) unit body
    forkAff do
      Tuple old new <- matchesAff pRoute
      io.query $ H.action $ Root.Goto new
    forkAff $ forever do
      cfg <- takeVar notifications
      io.query $ H.action $ Root.Notify cfg
