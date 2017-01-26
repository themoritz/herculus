{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           React.Flux      (executeAction, reactRender)
import           React.Flux.Ajax (initAjax)

import qualified Config
import           Store           (Action (Init), dispatch)
import           Views           (app)

main :: IO ()
main = do
  initAjax
  executeAction $ head $ dispatch $ Init Config.webSocketUrl
  reactRender "app" app ()
