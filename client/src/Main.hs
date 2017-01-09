{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           React.Flux      (executeAction, reactRender)
import           React.Flux.Ajax (initAjax)

import qualified Config
import           Store           (Action (GlobalInit), dispatch)
import           Views           (app)

main :: IO ()
main = do
  initAjax
  executeAction $ head $ dispatch $ GlobalInit Config.webSocketUrl
  reactRender "app" app ()
