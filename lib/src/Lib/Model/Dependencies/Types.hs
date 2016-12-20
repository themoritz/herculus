{-# LANGUAGE DeriveGeneric #-}

module Lib.Model.Dependencies.Types where

import           Data.Monoid
import           Data.Serialize
import           Data.Set         (Set)
import qualified Data.Set         as Set

import           GHC.Generics

import {-# SOURCE #-}           Lib.Model.Column
import {-# SOURCE #-}           Lib.Model.Table
import           Lib.Types

data ColumnDependant
  = ColDepRef
  | ColDepWholeRef
  deriving (Generic, Eq, Ord, Show)

instance Serialize ColumnDependant

data TableDependant
  = TblDepColumnRef
  | TblDepTableRef
  | TblDepRowRef
  deriving (Generic, Eq, Ord, Show)

instance Serialize TableDependant

data AddTargetMode
  = AddOne
  | AddAll

-- | Kinds of code dependencies (from the view of a column)
data CodeDependencies = CodeDependencies
  { codeDepColumnRefs      :: Set (Id Column)
  , codeDepWholeColumnRefs :: Set (Id Table, Id Column)
  , codeDepTableRefs       :: Set (Id Table)
  }

instance Monoid CodeDependencies where
  mempty = CodeDependencies Set.empty Set.empty Set.empty
  mappend (CodeDependencies c1 w1 t1) (CodeDependencies c2 w2 t2) =
    CodeDependencies (c1 <> c2) (w1 <> w2) (t1 <> t2)

columnsOfCodeDeps :: CodeDependencies -> Set (Id Column)
columnsOfCodeDeps = undefined

tablesOfCodeDeps :: CodeDependencies -> Set (Id Table)
tablesOfCodeDeps = undefined

singleColumnRef :: Id Column -> CodeDependencies
singleColumnRef columnId =
  CodeDependencies (Set.singleton columnId) Set.empty Set.empty

singleWholeColumnRef :: Id Table -> Id Column -> CodeDependencies
singleWholeColumnRef tableId columnId =
  CodeDependencies Set.empty (Set.singleton (tableId, columnId)) Set.empty

singleTableRef :: Id Table -> CodeDependencies
singleTableRef tableId =
  CodeDependencies Set.empty Set.empty (Set.singleton tableId)

-- | Kinds of type dependencies (from the view of a column)
data TypeDependencies = TypeDependencies
  { typeDepsRowRef :: Set (Id Table)
  }

instance Monoid TypeDependencies where
  mempty = TypeDependencies Set.empty
  mappend (TypeDependencies r1) (TypeDependencies r2) =
    TypeDependencies (r1 <> r2)

singleRowRef :: Id Table -> TypeDependencies
singleRowRef tableId = TypeDependencies (Set.singleton tableId)
