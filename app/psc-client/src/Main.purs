module Main where

import Prelude
import Halogen as H
import Api.Rest (SPParams_(SPParams_))
import App (app)
import Control.Monad.Eff (Eff)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Servant.PureScript.Settings (defaultSettings)
import Types (runAppM, AppEffects)

main :: Eff (AppEffects ()) Unit
main = runHalogenAff $ do
  body <- awaitBody
  let settings = defaultSettings $ SPParams_
        { baseURL: "api/"
        }
  runUI (H.hoist (runAppM settings) app) unit body
