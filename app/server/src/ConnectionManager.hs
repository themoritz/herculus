{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module ConnectionManager
  ( ConnectionId
  , ConnectionManager
  , mkConnectionManager
  , addConnection
  , removeConnection
  , authUser
  , forgetUser
  , subscribeProject
  , unsubscribe
  , getUser
  , getConnection
  , getConnectionsToProject
  ) where


import           Control.Lens
import           Control.Monad.State

import           Data.Map                      (Map)
import qualified Data.Map                      as Map

import           Network.WebSockets.Connection

import           Lib.Model.Auth                (User)
import           Lib.Model.Project
import           Lib.Types

type ConnectionId = Int

data ConnectionManager = ConnectionManager
  { _connections :: Map ConnectionId ConnectionInfo
  , _nextId      :: ConnectionId
  } deriving (Show)

mkConnectionManager :: ConnectionManager
mkConnectionManager = ConnectionManager Map.empty 0

data ConnectionInfo = ConnectionInfo
  { _connection :: Connection
  , _userInfo   :: Maybe UserInfo
  }

instance Show ConnectionInfo where
  show (ConnectionInfo _ ui) = show ui

mkConnectionInfo :: Connection -> ConnectionInfo
mkConnectionInfo c = ConnectionInfo c Nothing

data UserInfo = UserInfo
  { _userId       :: Id User
  , _subscription :: Maybe (Id Project)
  } deriving (Show)

mkUserInfo :: Id User -> UserInfo
mkUserInfo u = UserInfo u Nothing

makeLenses ''ConnectionManager
makeLenses ''ConnectionInfo
makeLenses ''UserInfo

addConnection :: MonadState ConnectionManager m => Connection -> m ConnectionId
addConnection c = do
  i <- nextId <+= 1
  connections . at i .= Just (mkConnectionInfo c)
  pure i

removeConnection :: MonadState ConnectionManager m => ConnectionId -> m ()
removeConnection i = connections . at i .= Nothing

authUser :: MonadState ConnectionManager m => ConnectionId -> Id User -> m ()
authUser i u = connections . at i . _Just . userInfo .= Just (mkUserInfo u)

forgetUser :: MonadState ConnectionManager m => ConnectionId -> m ()
forgetUser i = connections . at i . _Just . userInfo .= Nothing

subscribeProject :: MonadState ConnectionManager m
                 => ConnectionId -> Id Project -> m ()
subscribeProject c p =
  connections . at c . _Just . userInfo . _Just . subscription .= Just p

unsubscribe :: MonadState ConnectionManager m => ConnectionId -> m ()
unsubscribe c =
  connections . at c . _Just . userInfo . _Just . subscription .= Nothing

getUser :: MonadState ConnectionManager m => ConnectionId -> m (Maybe (Id User))
getUser c = do
  cs <- use connections
  pure $ cs ^? at c . _Just . userInfo . _Just . userId

getConnection :: MonadState ConnectionManager m
              => ConnectionId -> m (Maybe Connection)
getConnection c = do
  cs <- use connections
  pure $ cs ^? at c . _Just . connection

getConnectionsToProject :: MonadState ConnectionManager m
                        => Id Project -> m [ConnectionId]
getConnectionsToProject p =
  uses connections $ Map.keys . Map.filter (\info ->
    info ^? userInfo . _Just . subscription . _Just == Just p)
