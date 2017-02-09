module Herculus.Monad where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen (ComponentDSL, lift)
import Halogen.Aff (HalogenEffects)
import Lib.Api.Rest (SPParams_(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import WebSocket (WEBSOCKET)

type AppEffects eff = HalogenEffects
  ( "ajax"    :: AJAX
  , "console" :: CONSOLE
  , "ws"      :: WEBSOCKET
  | eff
  )

type BaseUrl = String
type AuthToken = Maybe String

-- | App monad. Has reader access to servant-purescript params.
type AppM eff =
  StateT AuthToken (ReaderT BaseUrl (Aff (AppEffects eff)))

runAppM
  :: forall eff
   . BaseUrl
  -> (AppM eff ~> Aff (AppEffects eff))
runAppM env = \action ->
  runReaderT (evalStateT action Nothing) env

getAuthToken :: forall s i o eff. ComponentDSL s i o (AppM eff) AuthToken
getAuthToken = lift get

setAuthToken :: forall s i o eff. String -> ComponentDSL s i o (AppM eff) Unit
setAuthToken = lift <<< put <<< Just

type ApiT m
  = ExceptT AjaxError (ReaderT (SPSettings_ SPParams_) m)

runApiT
  :: forall m a
   . BaseUrl -> AuthToken
  -> ApiT m a -> m (Either AjaxError a)
runApiT baseUrl mToken action =
  let
    settings = defaultSettings $ SPParams_
      { baseURL: baseUrl
      , authorization: fromMaybe "Nothing" mToken
      }
  in
    runReaderT (runExceptT action) settings

type Env =
  { withApi
      :: forall s i o eff a
       . ApiT (AppM eff) a
      -> (a -> ComponentDSL s i o (AppM eff) Unit)
      -> ComponentDSL s i o (AppM eff) Unit
  , notify
      :: forall s i o eff
       . String -> ComponentDSL s i o (AppM eff) Unit
  }
