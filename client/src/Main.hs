{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import React.Flux
import React.Flux.Ajax

import Store
import Views

main :: IO ()
main = do
  fu
  initAjax
  executeAction $ SomeStoreAction store $ GlobalInit "ws://localhost:3000/websocket"
  reactRender "app" app ()
