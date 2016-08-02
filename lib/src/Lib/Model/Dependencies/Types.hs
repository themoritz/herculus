{-# LANGUAGE DeriveGeneric #-}

module Lib.Model.Dependencies.Types where

import Data.Aeson

import GHC.Generics

data DependencyType
  = OneToOne
  | OneToAll
  deriving (Generic, Eq, Ord, Show)

instance ToJSON DependencyType
instance FromJSON DependencyType
