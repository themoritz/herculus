{-# LANGUAGE DeriveGeneric #-}

module Lib.Expression where

import Data.Aeson
import Lib

import GHC.Generics

data Expression
  = ExprColumnRef (Ref Table) (Ref Column)
  deriving (Show, Generic)

instance ToJSON Expression
instance FromJSON Expression
