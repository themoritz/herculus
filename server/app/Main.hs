{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Data.Proxy
import qualified Data.Text.IO             as Text

import           Servant
import           Servant.API

import           Network.Wai.Handler.Warp as Warp

import           Lib
import           Server

type Api = "command" :> Get '[JSON] Command

api :: Proxy Api
api = Proxy

server :: Server Api
server = pure $ CommandNew "Hello"

main :: IO ()
main = do
  Text.putStrLn foo
  Warp.run 3000 $ serve api server
