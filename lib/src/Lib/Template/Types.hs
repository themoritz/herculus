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

newtype CTemplate = CTemplate [CTplExpr]
  deriving (Eq, Show, Generic, NFData)

instance ToJSON CTemplate
instance FromJSON CTemplate

-- Compiled template using core expressions
data CTplExpr
  = CTplText Text
  | CTplFor Name CExpr CTemplate
  | CTplIf CExpr CTemplate CTemplate
  | CTplShow CExpr
  deriving (Eq, Show, Generic, NFData)

instance ToJSON CTplExpr
instance FromJSON CTplExpr

collectTplDependencies :: CTemplate -> [(Id Column, DependencyType)]
collectTplDependencies = go
  where go (CTemplate tpls) = concatMap goOne tpls
        goOne e' = case e' of
          CTplText _ -> []
          CTplFor _ e body -> collectDependencies e <> go body
          CTplIf e then' else' -> collectDependencies e <> go then' <> go else'
          CTplShow e -> collectDependencies e
