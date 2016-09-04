{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Template.Types where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Text

import           GHC.Generics

import           Lib.Compiler.Types
import           Lib.Types

newtype PTemplate = PTemplate [PTplExpr]
  deriving (Show)

data PTplExpr
  = PTplText Text
  | PTplFor Name PExpr PTemplate
  | PTplIf PExpr PTemplate PTemplate
  | PTplShow PExpr
  deriving (Show)

newtype TTemplate = TTemplate [TTplExpr]
  deriving (Eq, Show, Generic, NFData)

instance ToJSON TTemplate
instance FromJSON TTemplate

data TTplExpr
  = TTplText Text
  | TTplFor Name TExpr TTemplate
  | TTplIf TExpr TTemplate TTemplate
  | TTplShow TExpr
  deriving (Eq, Show, Generic, NFData)

instance ToJSON TTplExpr
instance FromJSON TTplExpr
