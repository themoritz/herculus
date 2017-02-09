module Main where

import Prelude
import Halogen as H
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (newRef)
import Data.Maybe (Maybe(..))
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Herculus.App (app)
import Herculus.Monad (runHerc, HercEffects)

main :: Eff HercEffects Unit
main = do
  tokenRef <- newRef Nothing
  runHalogenAff $ do
    body <- awaitBody
    let
      wiring =
        { baseUrl: "api/"
        , authTokenRef: tokenRef
        }
    runUI (H.hoist (runHerc wiring) app) unit body
