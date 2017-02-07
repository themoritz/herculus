module Play where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lib.Api.Rest as Api
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (ExceptT)
import Data.Array (head)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing, Just))
import Lib.Api.Schema.Auth (LoginData(..), LoginResponse(..), uiSessionKey, uiUserName)
import Lib.Api.Schema.Project (Project(Project), ProjectData(ProjectData))
import Lib.Model (Entity(..))
import Lib.Model.Auth (Email(..))
import Servant.PureScript.Affjax (AjaxError)
import Types (AppM)

type State = Unit

data Query a
  = Init a
  | Foo a


type Env =
  { runApi :: forall s i o eff
            . ExceptT AjaxError (H.ComponentDSL s i o (AppM eff)) Unit
           -> H.ComponentDSL s i o (AppM eff) Unit
  }

play :: forall i o eff. Env -> H.Component HH.HTML Query i o (AppM eff)
play env = H.lifecycleComponent
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
        [ HH.button
          [ HE.onClick (HE.input_ Foo) ]
          [ HH.text "Foo" ]
        ]
    
    eval :: Query ~> H.ComponentDSL State Query o (AppM eff)
    eval (Init next) = do
      env.runApi do
        let loginData = LoginData
              { ldEmail: Email { unEmail: "mdrexl@fastmail.fm" }
              , ldPassword: "Andreas"
              }

        loginResult <- Api.postAuthLogin loginData
        case loginResult of
          LoginFailed e -> liftAff $ log e
          LoginSuccess userInfo -> do
            liftAff $ log $ userInfo ^. uiUserName
            let auth = userInfo ^. uiSessionKey
            ps <- Api.getProjectList auth
            case head ps of
              Nothing -> pure unit
              Just (Project p) -> do
                liftAff $ log $ p._projectName
                ProjectData pd <- Api.getProjectLoadByProjectId auth p._projectId
                case head pd._pdTables of
                  Nothing -> pure unit
                  Just (Entity e) -> do
                    liftAff $ log "Done."
      pure next
    eval (Foo next) = do
      liftAff $ log "Foo"
      pure next
