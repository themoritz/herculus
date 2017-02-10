module Herculus.Monad where

import Herculus.Prelude

import Ace.Types (ACE)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (Ref, readRef, writeRef)
import Control.Monad.Free (Free, foldFree, liftF)
import Halogen (ComponentDSL)
import Halogen.Aff (HalogenEffects)
import Lib.Api.Rest (SPParams_(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import WebSocket (WEBSOCKET)

type HercEffects = HalogenEffects
  ( ajax    :: AJAX
  , ace     :: ACE
  , console :: CONSOLE
  , ws      :: WEBSOCKET
  )

type Url = String
type AuthToken = Maybe String

type Wiring =
  { apiUrl :: Url
  , webSocketUrl :: Url
  , authTokenRef :: Ref AuthToken
  }

type Herc = HercM HercEffects

data HercF eff a
  = Aff (Aff eff a)
  | GetAuthToken (AuthToken -> a)
  | SetAuthToken AuthToken a
  | GetApiUrl (Url -> a)
  | GetWebSocketUrl (Url -> a)

newtype HercM eff a = HercM (Free (HercF eff) a)

unHercM :: forall eff. HercM eff ~> Free (HercF eff)
unHercM (HercM m) = m

derive newtype instance functorHercM :: Functor (HercM eff)
derive newtype instance applyHercM :: Apply (HercM eff)
derive newtype instance applicativeHercM :: Applicative (HercM eff)
derive newtype instance bindHercM :: Bind (HercM eff)
derive newtype instance monadHercM :: Monad (HercM eff)

instance monadEffHercM :: MonadEff eff (HercM eff) where
  liftEff = HercM <<< liftF <<< Aff <<< liftEff

instance monadAffHercM :: MonadAff eff (HercM eff) where
  liftAff = HercM <<< liftF <<< Aff

getAuthToken :: Herc AuthToken
getAuthToken = HercM $ liftF $ GetAuthToken id

setAuthToken :: String -> Herc Unit
setAuthToken t = HercM $ liftF $ SetAuthToken (Just t) unit

getApiUrl :: Herc Url
getApiUrl = HercM $ liftF $ GetApiUrl id

getWebSocketUrl :: Herc Url
getWebSocketUrl = HercM $ liftF $ GetWebSocketUrl id

--------------------------------------------------------------------------------

runHerc :: Wiring -> Herc ~> Aff HercEffects
runHerc { apiUrl, webSocketUrl, authTokenRef } = foldFree go <<< unHercM
  where

  go :: HercF HercEffects ~> Aff HercEffects
  go = case _ of
    Aff aff ->
      aff
    GetAuthToken reply -> do
      token <- liftEff $ readRef authTokenRef
      pure (reply token)
    SetAuthToken token next -> do
      liftEff $ writeRef authTokenRef token
      pure next
    GetApiUrl reply -> pure (reply apiUrl)
    GetWebSocketUrl reply -> pure (reply webSocketUrl)

--------------------------------------------------------------------------------

type ApiT m
  = ExceptT AjaxError (ReaderT (SPSettings_ SPParams_) m)

runApiT
  :: forall m a
   . Url -> AuthToken
  -> ApiT m a -> m (Either AjaxError a)
runApiT apiUrl mToken action =
  let
    settings = defaultSettings $ SPParams_
      { baseURL: apiUrl
      , authorization: fromMaybe "Nothing" mToken
      }
  in
    runReaderT (runExceptT action) settings

type HercEnv =
  { withApi
      :: forall s i o a
       . ApiT Herc a
      -> (a -> ComponentDSL s i o Herc Unit)
      -> ComponentDSL s i o Herc Unit
  , notify
      :: forall s i o
       . String -> ComponentDSL s i o Herc Unit
  }
