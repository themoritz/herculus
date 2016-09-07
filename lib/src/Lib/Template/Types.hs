{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Template.Types where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Monoid
import           Data.Text (Text)

import           GHC.Generics

import           Lib.Compiler.Types
import {-# SOURCE #-} Lib.Model.Column
import           Lib.Model.Dependencies.Types
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

collectTplDependencies :: TTemplate -> [(Id Column, DependencyType)]
collectTplDependencies = go
  where go (TTemplate tpls) = concatMap goOne tpls
        goOne e' = case e' of
          TTplText _ -> []
          TTplFor _ e body -> collectDependencies e <> go body
          TTplIf e then' else' -> collectDependencies e <> go then' <> go else'
          TTplShow e -> collectDependencies e
