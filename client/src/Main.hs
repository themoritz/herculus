{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.DeepSeq
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Typeable (Typeable)

import GHC.Generics (Generic)

import React.Flux
import React.Flux.Ajax
import React.Flux.Addons.Servant

import Lib.Types
import Lib.Model
import Lib.Model.Types

import Lib.Api.Rest

import WebSocket

main :: IO ()
main = do
  initAjax
  reactRender "app" test ()

data RendererArgs = RendererArgs Int Bool

instance FromJSON RendererArgs where
  parseJSON (Object o) = RendererArgs <$> o .: "index"
                                      <*> o .: "isScrolling"
  parseJSON _ = mempty

test :: ReactView ()
test = defineControllerView "test" store $ \(State ps) () -> do
  h1_ "Test"
  button_ [ onClick $ \_ _ -> dispatch Load ] "Load"
  let huge = join $ replicate 10000 ps
      toProps :: Value -> ReturnProps (Project, Bool)
      toProps v = case parseMaybe parseJSON v of
        Nothing -> ReturnProps (entityVal $ huge !! 0, False)
        Just (RendererArgs index scrolling) -> ReturnProps (entityVal $ huge !! index, scrolling)
  foreign_ "VirtualScroll"
    [ "width" &= (300 :: Int)
    , "height" &= (300 :: Int)
    , "rowCount" &= length huge
    , "rowHeight" &= (60 :: Int)
    , callbackViewWithProps "rowRenderer" project toProps
    ] mempty

project :: ReactView (Project, Bool)
project = defineView "project" $ \(p, b) -> do
  if b then "scrolling" else ""
  elemText (projectName p)

data State = State [Entity Project]

data Action
  = Load
  | Set [Entity Project]
  deriving (Typeable, Generic, NFData)

api :: ApiRequestConfig Routes
api = ApiRequestConfig "" NoTimeout

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

instance StoreData State where
  type StoreAction State = Action
  transform action st@(State _) = case action of
    Load -> do
      request api (Proxy :: Proxy ProjectList) $ \case
        Left _ -> pure []
        Right ps -> pure $ dispatch $ Set ps
      pure st
    Set ps -> pure $ State ps

store :: ReactStore State
store = mkStore $ State []
