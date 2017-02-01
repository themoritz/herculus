{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Model.Dependencies.Types where

import           Data.Foldable    (for_)
import           Data.Monoid
import           Data.Serialize
import           Data.Set         (Set)
import qualified Data.Set         as Set

import           GHC.Generics

import {-# SOURCE #-} Lib.Model.Column
import {-# SOURCE #-} Lib.Model.Table
import           Lib.Types

-- | A dependency that goes from column to column.
data ColumnDependency
  = ColDepRef
  -- ^ If `$A` is written into column C, we have this from A -> C.
  | ColDepWholeRef
  -- ^ If `#T.A` is written into column C, we have this from A -> C.
  deriving (Generic, Eq, Ord, Show)

instance Serialize ColumnDependency

-- | A dependency that goes from table to column.
data TableDependency
  = TblDepColumnRef
  -- ^ If `#T.A` is written into column C, we have this from T -> C.
  | TblDepTableRef
  -- ^ If `#T` is written into column C, we have this from T -> C.
  | TblDepRowRef
  -- ^ If column C is of type `Row T`, we have this from T -> C.
  deriving (Generic, Eq, Ord, Show)

instance Serialize TableDependency

data AddTargetMode
  = AddOne
  | AddAll
  deriving (Show)

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
columnsOfCodeDeps CodeDependencies{..} =
  codeDepColumnRefs `Set.union` Set.map snd codeDepWholeColumnRefs

tablesOfCodeDeps :: CodeDependencies -> Set (Id Table)
tablesOfCodeDeps CodeDependencies{..} =
  codeDepTableRefs `Set.union` Set.map fst codeDepWholeColumnRefs

singleColumnRef :: Id Column -> CodeDependencies
singleColumnRef columnId =
  CodeDependencies (Set.singleton columnId) Set.empty Set.empty

singleWholeColumnRef :: Id Table -> Id Column -> CodeDependencies
singleWholeColumnRef tableId columnId =
  CodeDependencies Set.empty (Set.singleton (tableId, columnId)) Set.empty

singleTableRef :: Id Table -> CodeDependencies
singleTableRef tableId =
  CodeDependencies Set.empty Set.empty (Set.singleton tableId)

forCodeDeps_ :: Monad m => CodeDependencies
             -> (Either (Id Column, ColumnDependency) (Id Table, TableDependency) -> m ()) -> m ()
forCodeDeps_ CodeDependencies{..} f = do
  for_ (Set.toList codeDepColumnRefs) $ \c -> f $ Left (c, ColDepRef)
  for_ (Set.toList codeDepWholeColumnRefs) $ \(t, c) -> do
    f $ Left (c, ColDepWholeRef)
    f $ Right (t, TblDepColumnRef)
  for_ (Set.toList codeDepTableRefs) $ \t -> f $ Right (t, TblDepTableRef)

-- | Kinds of type dependencies (from the view of a column)
data TypeDependencies = TypeDependencies
  { typeDepsRowRef :: Set (Id Table)
  }

tablesOfTypeDeps :: TypeDependencies -> Set (Id Table)
tablesOfTypeDeps = typeDepsRowRef

instance Monoid TypeDependencies where
  mempty = TypeDependencies Set.empty
  mappend (TypeDependencies r1) (TypeDependencies r2) =
    TypeDependencies (r1 <> r2)

singleRowRef :: Id Table -> TypeDependencies
singleRowRef tableId = TypeDependencies (Set.singleton tableId)
