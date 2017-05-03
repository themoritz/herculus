module Herculus.Monad where

import Herculus.Prelude
import Herculus.Notifications.Types as Notify
import Ace.Types (ACE)
import Control.Monad.Aff.AVar (AVar, putVar)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free (Free, foldFree, liftF)
import DOM (DOM)
import Data.JSDate (LOCALE)
import Data.Nullable (Nullable, toMaybe)
import Flatpickr.Types (FLATPICKR)
import Halogen.Aff (HalogenEffects)
import Herculus.Router (Root, setPath)
import Lib.Api.Rest (SPParams_(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError, ErrorDescription(ConnectionError, DecodingError, ParsingError, UnexpectedHTTPStatus), runAjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import WebSocket (WEBSOCKET)

foreign import data STORAGE :: Effect

type HercEffects = HalogenEffects
  ( ajax      :: AJAX
  , ace       :: ACE
  , console   :: CONSOLE
  , ws        :: WEBSOCKET
  -- | Only here because websocket-simple did not switch to `exception`.
  , err       :: EXCEPTION
  , storage   :: STORAGE
  , flatpickr :: FLATPICKR
  , locale    :: LOCALE
  )

type Url = String
type AuthToken = Maybe String

type Wiring =
  { apiUrl :: Url
  , webSocketUrl :: Url
  , notifications :: AVar Notify.Config
  }

type Herc = HercM HercEffects

data HercF eff a
  = Aff (Aff eff a)
  | GetAuthToken (AuthToken -> a)
  | SetAuthToken String a
  | Notify Notify.Config a
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

getAuthToken :: forall m. MonadTrans m => m Herc AuthToken
getAuthToken = lift $ HercM $ liftF $ GetAuthToken id

setAuthToken :: forall m. MonadTrans m => String -> m Herc Unit
setAuthToken t = lift $ HercM $ liftF $ SetAuthToken t unit

notify :: forall m. MonadTrans m => Notify.Config -> m Herc Unit
notify cfg = lift $ HercM $ liftF $ Notify cfg unit

getApiUrl :: forall m. MonadTrans m => m Herc Url
getApiUrl = lift $ HercM $ liftF $ GetApiUrl id

getWebSocketUrl :: forall m. MonadTrans m => m Herc Url
getWebSocketUrl = lift $ HercM $ liftF $ GetWebSocketUrl id

gotoRoute :: forall eff m. MonadEff (dom :: DOM | eff) m => Root -> m Unit
gotoRoute = liftEff <<< setPath

--------------------------------------------------------------------------------

foreign import basilSet
  :: forall eff. String -> String -> Eff (storage :: STORAGE | eff) Unit
foreign import basilGet
  :: forall eff. String -> Eff (storage :: STORAGE | eff) (Nullable String)

runHerc :: Wiring -> Herc ~> Aff HercEffects
runHerc wiring = foldFree go <<< unHercM
  where

  go :: HercF HercEffects ~> Aff HercEffects
  go = case _ of
    Aff aff ->
      aff
    GetAuthToken reply -> do
      token <- liftEff $ basilGet "sessionKey"
      pure (reply $ toMaybe token)
    SetAuthToken token next -> do
      liftEff $ basilSet "sessionKey" token
      pure next
    Notify cfg next -> do
      putVar wiring.notifications cfg
      pure next
    GetApiUrl reply -> pure (reply wiring.apiUrl)
    GetWebSocketUrl reply -> pure (reply wiring.webSocketUrl)

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

withApi
  :: forall m a
   . MonadTrans m => Monad (m Herc)
  => ApiT Herc a
  -> (a -> m Herc Unit)
  -> m Herc Unit
withApi call handler = do
  apiUrl <- getApiUrl
  token <- getAuthToken
  result <- lift $ runApiT apiUrl token call
  case result of
    Left e -> do
      let
        description = (runAjaxError e).description
      case description of
        UnexpectedHTTPStatus response -> 
          notify
            { kind: Notify.Error
            , message: response.response
            , detail: Nothing
            }
        ParsingError msg ->
          notify
            { kind: Notify.Error
            , message: "Api request failed."
            , detail: Just $ "Could not parse json message from server: " <> msg
            }
        DecodingError msg -> 
          notify
            { kind: Notify.Error
            , message: "Api request failed."
            , detail: Just $ "Could not decode json message from server: " <> msg
            }
        ConnectionError msg ->
          notify
            { kind: Notify.Error
            , message: "Api request failed."
            , detail: Just msg
            }
    Right a -> handler a
