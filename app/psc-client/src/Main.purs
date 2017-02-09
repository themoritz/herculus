module Main where

import Prelude
import Halogen as H
import Control.Monad.Eff (Eff)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Herculus.App (app)
import Herculus.Monad (runAppM, AppEffects)

main :: Eff (AppEffects ()) Unit
main = runHalogenAff $ do
  body <- awaitBody
  runUI (H.hoist (runAppM "api/") app) unit body
