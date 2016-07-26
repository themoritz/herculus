{-# LANGUAGE TemplateHaskell #-}

module ConnectionManager
  ( ConnectionManager
  , newConnectionManager
  , addConnection
  , removeConnection
  , allConnections
  ) where


import           Control.Lens
import           Control.Monad.State

import           Data.Map                      (Map)
import qualified Data.Map                      as Map

import           Network.WebSockets.Connection

data ConnectionManager = ConnectionManager
  { _connections :: Map Int Connection
  , _nextId      :: Int
  }

newConnectionManager :: ConnectionManager
newConnectionManager = ConnectionManager Map.empty 0

makeLenses ''ConnectionManager

addConnection :: Connection -> ConnectionManager -> (Int, ConnectionManager)
addConnection c mgr = flip runState mgr $ do
  i <- nextId <+= 1
  connections %= Map.insert i c
  pure i

removeConnection :: Int -> ConnectionManager -> ConnectionManager
removeConnection i = over connections (Map.delete i)

allConnections :: ConnectionManager -> [Connection]
allConnections = Map.elems . _connections
