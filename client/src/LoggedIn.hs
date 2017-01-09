{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |

module LoggedIn where

import           Control.DeepSeq                (NFData)
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State            hiding (State)

import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Proxy
import           Data.Text                      (Text)

import           GHC.Generics

import           React.Flux.Addons.Servant      (request)
import           React.Flux.Addons.Servant.Auth (AuthenticateReq)

import           Lib.Api.Rest                   as Api
import           Lib.Model
import           Lib.Model.Auth                 (ChangePwdData (..),
                                                 ChangePwdResponse (..),
                                                 SessionKey, UserInfo (..))
import           Lib.Model.Project
import           Lib.Types

import qualified Action.Message                 as Message
import qualified Project
import           Store.Types

--------------------------------------------------------------------------------

data State = State
  { _stateUserInfo         :: UserInfo
  , _stateSessionKey       :: SessionKey
  , _stateSubState         :: SubState
  , _stateUserSettingsShow :: Bool
  } deriving (Show)

mkState :: SessionKey -> UserInfo -> State
mkState sKey userInfo = State
  { _stateUserInfo         = userInfo
  , _stateSessionKey       = sKey
  , _stateSubState         = ProjectOverview Map.empty
  , _stateUserSettingsShow = False
  }

data SubState
  = ProjectOverview ProjectOverviewState
  | ProjectDetail Project.State
  | ChangePasswordForm Bool
  -- ^ Bool flag: True: valid old password or initial
  --              False: invalid old password
  deriving (Show)

type ProjectOverviewState = Map (Id ProjectClient) ProjectClient

makeLenses ''State
makePrisms ''SubState

--------------------------------------------------------------------------------

data Action
  = Logout
  | ToggleUserSettingsDialog
  --
  | ToProjectOverview
  | ToProject (Id ProjectClient)
  | ToChangePasswordForm
  -- Change password
  | ChangePassword ChangePwdData
  -- Project overview
  | CreateProject Text
  | DeleteProject (Id ProjectClient)
  -- Project
  | ProjectAction Project.Action
  deriving (Generic, Show)

instance NFData Action

--------------------------------------------------------------------------------

data Env m = Env
  { accessState :: forall a. (State -> (a, State)) -> m a
  , toLoginForm :: m ()
  }

newtype DSL m a = DSL
  { runDSL :: ReaderT (Env m) m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Env m)
             )

instance Monad m => MonadState State (DSL m) where
  state f = do
    access <- asks accessState
    lift (access f)

instance MonadTrans DSL where
  lift = DSL . lift

instance MonadStore m => MonadStore (DSL m) where
  apiCall = lift . apiCall
  showMessage = lift . showMessage
  haltMessage = lift . haltMessage

instance MonadStore m => Project.HasProjectState (DSL m) where
  projectState f = do
    st <- get
    case st ^? stateSubState . _ProjectDetail of
      Just p -> do
        let (a, p') = f p
        stateSubState . _ProjectDetail .= p'
        pure a
      Nothing ->
        haltMessage "inconsistent client state. cannot access project detail"

run :: (MonadStore m)
    => Env m -> Action -> m ()
run env action = do
  let eval' = do
        sKey <- use stateSessionKey
        eval (session sKey) action
  runReaderT (runDSL eval') env

--------------------------------------------------------------------------------

eval :: (MonadStore m)
     => AuthenticateReq Api.SessionProtect
     -> Action -> DSL m ()
eval token = \case

  Logout -> do
    apiCall $ request api (Proxy :: Proxy Api.AuthLogout) token
    showMessage $ Message.SetSuccess "Successfully logged out."
    go <- asks toLoginForm
    lift go

  ToggleUserSettingsDialog ->
    stateUserSettingsShow %= not

  ToChangePasswordForm ->
    stateSubState .= ChangePasswordForm True

  ChangePassword changePwdData ->
    apiCall (request api (Proxy :: Proxy Api.AuthChangePassword)
                         token changePwdData
            ) >>= \case
      ChangePwdSuccess -> do
        showMessage $ Message.SetSuccess "Successfully changed password."
        eval token ToProjectOverview
      ChangePwdFailure msg -> do
        showMessage $ Message.SetWarning msg
        stateSubState .= ChangePasswordForm False

  -- Project Overview ----------------------------------------------------------

  ToProjectOverview -> do
    ps <- apiCall $ request api (Proxy :: Proxy Api.ProjectList) token
    stateSubState .= ProjectOverview (Map.fromList $ map entityToTuple ps)

  CreateProject name -> do
    Entity i p <- apiCall $ request api (Proxy :: Proxy Api.ProjectCreate)
                                      token name
    stateSubState .= ProjectDetail (Project.mkState i p)

  ToProject i -> do
    (p, ts) <- apiCall $ request api (Proxy :: Proxy Api.ProjectLoad)
               token i
    let tablesMap = Map.fromList $ map entityToTuple ts
    stateSubState .= ProjectDetail
      (Project.mkState i p & Project.stateTables .~ tablesMap)
    -- Load first table if exists
    case ts of
      []               -> pure ()
      Entity tId _ : _ -> eval token $ ProjectAction $ Project.LoadTable tId

  DeleteProject projectId -> do
    apiCall $ request api (Proxy :: Proxy Api.ProjectDelete) token projectId
    eval token ToProjectOverview

  ProjectAction action -> do
    sKey <- use stateSessionKey
    Project.run (session sKey) action
