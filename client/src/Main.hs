{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           React.Flux      (SomeStoreAction (..), executeAction,
                                  reactRender)
import           React.Flux.Ajax (initAjax)

import           Action          (Action (GlobalInit))
import qualified Config
import           Store           (store)
import           Views

main :: IO ()
main = do
  initAjax
  executeAction $ SomeStoreAction store $ GlobalInit Config.webSocketUrl
  reactRender "app" app ()
