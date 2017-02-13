module Herculus.Play where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Lib.Api.Rest as Api
import Control.Monad.Aff.Console (log)
import Data.Array (head)
import Herculus.Monad (Herc, notify, setAuthToken, withApi)
import Herculus.Notifications.Types as N
import Lib.Api.Schema.Auth (LoginData(LoginData), LoginResponse(LoginSuccess, LoginFailed), uiSessionKey, uiUserName)
import Lib.Api.Schema.Project (Project(Project), ProjectData(ProjectData))
import Lib.Model (Entity(..))
import Lib.Model.Auth (Email(..))

type State = Unit

data Query a
  = Init a
  | Foo a


play :: forall i o. H.Component HH.HTML Query i o Herc
play = H.lifecycleComponent
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
    
    eval :: Query ~> H.ComponentDSL State Query o Herc
    eval (Init next) = do
      notify
        { kind: N.Error
        , message: "Foo"
        , detail: Nothing
        }
      let loginData = LoginData
            { ldEmail: Email { unEmail: "mdrexl@fastmail.fm" }
            , ldPassword: "Andreas"
            }

      withApi (Api.postAuthLogin loginData) case _ of
        LoginFailed e -> liftAff $ log e
        LoginSuccess userInfo -> do
          liftAff $ log $ userInfo ^. uiUserName
          lift $ setAuthToken (userInfo ^. uiSessionKey)
          withApi Api.getProjectList \ps ->
          case head ps of
            Nothing -> pure unit
            Just (Project p) -> do
              liftAff $ log $ p._projectName
              withApi (Api.getProjectLoadByProjectId p._projectId) \(ProjectData pd) ->
              case head pd._pdTables of
                Nothing -> pure unit
                Just (Entity e) -> do
                  liftAff $ log "Done."
      pure next
    eval (Foo next) = do
      liftAff $ log "Foo"
      pure next
