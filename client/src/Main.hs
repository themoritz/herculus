{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           React.Flux             (SomeStoreAction (..), executeAction,
                                         reactRender)
import           React.Flux.Addons.Free
import           React.Flux.Ajax        (initAjax)

import           Action                 (Action (GlobalInit))
import qualified Config
import           Store                  (store)
import           Views                  (app)

main :: IO ()
main = do
  initAjax
  executeAction $ SomeStoreAction store $ freeFluxDispatch $ GlobalInit Config.webSocketUrl
  reactRender "app" app ()
