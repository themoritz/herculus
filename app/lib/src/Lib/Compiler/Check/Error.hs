{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Check.Error
  ( CheckError (..)
  , checkError
  , CheckAppendError (..)
  , checkAppendError
  ) where

import           Lib.Prelude

import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

import           Lib.Compiler.AST.Position
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

checkAppendError :: MonadError Error m => Error -> CheckAppendError -> m a
checkAppendError err err' = throwError $ err
  { errMsg = errMsg err <> "\n\n" <> printCheckAppendErr err'
  }

checkError :: MonadError Error m => Span -> CheckError -> m a
checkError span err = compileError span $ printCheckErr err

-- Always expected first
data CheckError
  = UndefinedTypeVariable Text
  | UndefinedTypeConstructor Text
  | UndefinedVariable Text
  | UndefinedConstructor Text
  | UndefinedClass Text
  | UndefinedMethod Text Text
  | ExpectedFunction Type
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
  | KindMismatch Kind Kind
  | TypeMismatch Type Type
  | FieldMismatch Type Type Text
  | InfiniteKind Kind Kind
  | InfiniteType Type Type

printCheckErr :: CheckError -> Text
printCheckErr = \case
  UndefinedTypeVariable v ->
    "Undefined type variable: " <> v
  UndefinedTypeConstructor c ->
    "Undefined type constructor: " <> c
  UndefinedVariable x ->
    "Undefined variable: " <> x
  UndefinedConstructor c ->
    "Undefined constructor: " <> c
  UndefinedClass cls ->
    "Undefined class: " <> cls
  UndefinedMethod name cls ->
    "Method `" <> name <> "` is not a member of class `" <> cls <> "`."
  ExpectedFunction t ->
    "Expected function but got `" <> prettyType t <> "`."
  ExpectedRowOfTable t ->
    "Expected row of table but got `" <> prettyType t <> "`."
  UnknownTable t ->
    "Table not found: " <> show t
  UnknownColumn c ->
    "Column not found: " <> show c
  UnknownColumnOfTable c t ->
    "Table `" <> show t <> "` does not have a column named `" <> show c <> "`."
  SignatureMissingConstraints cs ->
    "Not enough constraints given in the type signature. " <>
    "Missing constraints that were inferred: `" <>
    prettyConstraints cs <> "`."
  SignatureTooGeneral inferred given ->
    "The given type signature is too general. The inferred type is `" <>
    prettyPolyType inferred <> "` while the given signature was `" <>
    prettyPolyType given <> "`."
  SignatureMustBeUnconstrained name ->
    "Class methods are not allowed to be constrained."
  WrongNumberOfConstructorArgs c expected actual ->
    "Type constructor `" <> c <>
    "` expects " <> show expected <>
    " arguments but was given " <> show actual <> "."
  TypeDoesNotHaveFields t ->
    "Won't be able to access fields of type `" <> prettyType t <> "`."
  MissingSuperclassInstance cls ->
    "No instance for superclass `" <> cls <> "` found."
  MissingInstance cls t ->
    "The type `" <> prettyType t <>
    "` does not implement class `" <> cls <> "`."
  MissingField f record ->
    "Could not verify object has required field `" <> f <> "`."
  MissingImplementation name ->
    "No implementation provided for `" <> name <> "`."
  NoHeadNormalForm t ->
    "Constraints must be in head-normal form (starting with a type variable)."
  InvalidInstanceHeadType t ->
    "The type appearing in an instance head must be a type constructor."
  InvalidInstanceConstraintType t ->
    "Type of instance constraint must be type variable."
  OverlappingInstance otherType ->
    "This instance overlaps with an instance of type `" <>
    prettyType otherType <> "`."
  DuplicateClass cls ->
    "Class `" <> cls <> "` is already defined"
  KindMismatch expected actual ->
    "Cannot match kind `" <>
    prettyKind expected <> "` with `" <>
    prettyKind actual <> "`."
  TypeMismatch expected actual ->
    "Cannot match type `" <>
    prettyType expected <> "` with `" <>
    prettyType actual <> "`."
  FieldMismatch expected actual field ->
    "Cannot unify record types `" <>
    prettyType expected <> "` with `" <>
    prettyType actual <> "` because of field `" <> field <> "`."
  InfiniteKind expected actual ->
    "Infinite kind while trying to match `" <>
    prettyKind expected <> "` with `" <>
    prettyKind actual <> "`."
  InfiniteType expected actual ->
    "Infinite type while trying to unify `" <>
    prettyType expected <> "` with `" <>
    prettyType actual <> "`."

data CheckAppendError
  = CheckingSubsumption (Map Text Type) (Map Text Type) Text

printCheckAppendErr :: CheckAppendError -> Text
printCheckAppendErr = \case
  CheckingSubsumption big small field ->
    "While checking field `" <> field <> "`."
