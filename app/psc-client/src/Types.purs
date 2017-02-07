module Types where

import Prelude
import Lib.Api.Rest (SPParams_)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Reader (runReaderT, ReaderT)
import Halogen.Aff (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Settings (SPSettings_)

type AppEffects eff = HalogenEffects
  ( "ajax"    :: AJAX
  , "console" :: CONSOLE
  | eff
  )

-- | App monad. Has reader access to servant-purescript params.
type AppM eff =
  ReaderT (SPSettings_ SPParams_) (Aff (AppEffects eff))

runAppM
  :: forall eff
   . SPSettings_ SPParams_
  -> (AppM eff ~> Aff (AppEffects eff))
runAppM env = \action ->
  runReaderT action env
