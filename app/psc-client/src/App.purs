module App where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Lib.Api.Rest as Api
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (modify)
import Data.Array (head, length)
import Data.Either (Either(Right, Left))
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing, Just))
import Lib.Api.Schema.Auth (LoginData(..), LoginResponse(..), uiSessionKey, uiUserName)
import Lib.Api.Schema.Project (Project(..))
import Lib.Model.Auth (Email(..))
import Lib.Types (Id(..))
import Servant.PureScript.Affjax (AjaxError, errorToString)
import Types (AppM)

type State = Unit

data Query a = Init a

app :: forall i o eff. H.Component HH.HTML Query i o (AppM eff)
app = H.lifecycleComponent
    { initialState: const unit
    , receiver: const Nothing
    , render
    , eval
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }
  where

    render :: State -> H.ComponentHTML Query
    render st = HH.div_
        [
        ]
    
    eval :: Query ~> H.ComponentDSL State Query o (AppM eff)
    eval (Init next) = do
      let loginData = LoginData
            { ldEmail: Email { unEmail: "mdrexl@fastmail.fm" }
            , ldPassword: "Andreas"
            }

      apiCall (Api.postAuthLogin loginData) $ case _ of
        LoginFailed e -> liftAff $ log e
        LoginSuccess userInfo -> do
          liftAff $ log $ userInfo ^. uiUserName
          let auth = userInfo ^. uiSessionKey
          apiCall (Api.getProjectList auth) $ \ps -> case head ps of
            Nothing -> pure unit
            Just (Project p) -> do
              apiCall (Api.getProjectLoadByProjectId auth p._projectId) $ \result ->
                liftAff $ log $ p._projectName

      pure next

-- Utils -----------------------------------------------------------------------

-- | Logs `AjaxError`s to the console.
apiCall
  :: forall eff s i o a
   . ExceptT AjaxError (AppM eff) a
  -> (a -> H.ComponentDSL s i o (AppM eff) Unit)
  -> H.ComponentDSL s i o (AppM eff) Unit
apiCall call handler = do
  result <- H.lift $ runExceptT call
  case result of
    Left err -> liftAff $ log $ errorToString err
    Right a -> handler a
