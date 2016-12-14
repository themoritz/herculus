{-# LANGUAGE DeriveGeneric #-}

module Lib.Model.Dependencies.Types where

import           Data.Serialize

import           GHC.Generics

data ColumnDependency
  = ColDepRef
  | ColDepWholeRef
  deriving (Generic, Eq, Ord, Show)

instance Serialize ColumnDependency

data TableDependency
  = TblDepColumnRef
  | TblDepTableRef
  | TblDepRow
  deriving (Generic, Eq, Ord, Show)

instance Serialize TableDependency

data AddTargetMode
  = AddOne
  | AddAll
