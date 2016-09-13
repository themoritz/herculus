{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           React.Flux
import           React.Flux.Ajax

import qualified Config
import           Store
import           Views

main :: IO ()
main = do
  initAjax
  executeAction $ SomeStoreAction store $ GlobalInit Config.webSocketUrl
  reactRender "app" app ()
