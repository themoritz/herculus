-- |

module Lib.Compiler.Check.Error.Types where

import           Lib.Prelude

import           Lib.Compiler.Type
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

-- Always expected first
data CheckError
  = UndefinedTypeVariable Text
  | UndefinedTypeConstructor Text
  | UndefinedVariable Text
  | UndefinedConstructor Text
  | UndefinedClass Text
  | UndefinedMethod Text Text
  | ExpectedRowOfTable Type
  | UnknownTable (Ref Table)
  | UnknownColumn (Ref Column)
  | UnknownColumnOfTable (Ref Column) (Ref Table)
  | SignatureMissingConstraints [Constraint]
  | SignatureTooGeneral PolyType PolyType
  | SignatureMustBeUnconstrained Text
  | WrongNumberOfConstructorArgs Text Int Int
  | TypeDoesNotHaveFields Type
  | MissingSuperclassInstance Text
  | MissingInstance Text Type
  | MissingField Text (Map Text Type)
  | MissingImplementation Text
  | NoHeadNormalForm Type
  | InvalidInstanceHeadType Type
  | InvalidInstanceConstraintType Type
  | OverlappingInstance Type
  | DuplicateClass Text
  -- | Sub left and right, expected and actual
  | KindMismatch Kind Kind Kind Kind
  -- | Sub left and right, expected and actual
  | TypeMismatch Type Type Type Type
  | FieldMismatch Type Type Text
  | InfiniteKind Kind Kind
  | InfiniteType Type Type
  | AmbiguousTypeVar Text

traverseCheckError
  :: Monad m
  => (Kind -> m Kind)
  -> (Type -> m Type)
  -> (PolyType -> m PolyType)
  -> (Constraint -> m Constraint)
  -> CheckError -> m CheckError
traverseCheckError fKind fType fPoly fConstr = \case
  UndefinedTypeVariable v  ->
    pure $ UndefinedTypeVariable v
  UndefinedTypeConstructor c ->
    pure $ UndefinedTypeConstructor c
  UndefinedVariable v ->
    pure $ UndefinedVariable v
  UndefinedConstructor c ->
    pure $ UndefinedConstructor c
  UndefinedClass c ->
    pure $ UndefinedClass c
  UndefinedMethod name cls ->
    pure $ UndefinedMethod name cls
  ExpectedRowOfTable t ->
    ExpectedRowOfTable <$> fType t
  UnknownTable t ->
    pure $ UnknownTable t
  UnknownColumn c ->
    pure $ UnknownColumn c
  UnknownColumnOfTable c t ->
    pure $ UnknownColumnOfTable c t
  SignatureMissingConstraints cs ->
    SignatureMissingConstraints <$> traverse fConstr cs
  SignatureTooGeneral inferred given ->
    SignatureTooGeneral <$> fPoly inferred <*> fPoly given
  SignatureMustBeUnconstrained c ->
    pure $ SignatureMustBeUnconstrained c
  WrongNumberOfConstructorArgs c expected actual ->
    pure $ WrongNumberOfConstructorArgs c expected actual
  TypeDoesNotHaveFields t ->
    TypeDoesNotHaveFields <$> fType t
  MissingSuperclassInstance c ->
    pure $ MissingSuperclassInstance c
  MissingInstance c t ->
    MissingInstance c <$> fType t
  MissingField field r ->
    MissingField field <$> traverse fType r
  MissingImplementation c ->
    pure $ MissingImplementation c
  NoHeadNormalForm t ->
    NoHeadNormalForm <$> fType t
  InvalidInstanceHeadType t ->
    InvalidInstanceHeadType <$> fType t
  InvalidInstanceConstraintType t ->
    InvalidInstanceConstraintType <$> fType t
  OverlappingInstance t ->
    OverlappingInstance <$> fType t
  DuplicateClass c ->
    pure $ DuplicateClass c
  KindMismatch a b c d ->
    KindMismatch <$> fKind a <*> fKind b <*> fKind c <*> fKind d
  TypeMismatch a b c d ->
    TypeMismatch <$> fType a <*> fType b <*> fType c <*> fType d
  FieldMismatch a b field ->
    FieldMismatch <$> fType a <*> fType b <*> pure field
  InfiniteKind a b ->
    InfiniteKind <$> fKind a <*> fKind b
  InfiniteType a b ->
    InfiniteType <$> fType a <*> fType b
  AmbiguousTypeVar c ->
    pure $ AmbiguousTypeVar c
