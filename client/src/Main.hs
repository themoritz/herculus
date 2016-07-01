{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

import Lib

main :: IO ()
main = mainWidget widget

widget :: MonadWidget t m => m ()
widget = el "div" $ text $ show $ CommandNew "Hallo"
