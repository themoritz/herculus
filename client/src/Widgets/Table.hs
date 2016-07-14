{-# LANGUAGE TemplateHaskell #-}

module Widgets.Table
  ( table
  ) where

import Reflex.Dom

table :: MonadWidget t m => m ()
table = el "div" $ pure ()
