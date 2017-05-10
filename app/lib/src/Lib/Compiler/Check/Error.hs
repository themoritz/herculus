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
    "Interface methods are not allowed to be constrained."
  WrongNumberOfConstructorArgs c expected actual ->
    "Type constructor `" <> c <>
    "` expects " <> show expected <>
    " arguments but was given " <> show actual <> "."
  TypeDoesNotHaveFields t ->
    "Won't be able to access fields of type `" <> prettyType t <> "`."
  MissingSuperclassInstance cls ->
    "No implementation for interface `" <> cls <> "` found."
  MissingInstance cls t ->
    "The type `" <> prettyType t <>
    "` does not implement the interface `" <> cls <> "`."
  MissingField f record ->
    "Could not verify object has required field `" <> f <> "`."
  MissingImplementation name ->
    "No implementation provided for `" <> name <> "`."
  NoHeadNormalForm t ->
    "Constraints must be in head-normal form (starting with a type variable)."
  InvalidInstanceHeadType t ->
    "The type for which an interface is implemented must be a type constructor."
  InvalidInstanceConstraintType t ->
    "The types of implementation constraints must be type variables."
  OverlappingInstance otherType ->
    "This implementation overlaps with an implementation for type `" <>
    prettyType otherType <> "`."
  DuplicateClass cls ->
    "Interface `" <> cls <> "` is already defined"
  KindMismatch a b expected actual -> if a == expected && b == actual
    then "Cannot match expected kind `" <>
         prettyKind expected <> "` with actual kind `" <>
         prettyKind actual <> "`."
    else "Cannot match `" <>
         prettyKind a <> "` with `" <>
         prettyKind b <> "`. Expected kind is `" <>
         prettyKind expected <> "` while actual kind is `" <>
         prettyKind actual <> "`."
  TypeMismatch a b expected actual -> if a == expected && b == actual
    then "Cannot match expected type `" <>
         prettyType expected <> "` with actual type `" <>
         prettyType actual <> "`."
    else "Cannot match `" <>
         prettyType a <> "` with `" <>
         prettyType b <> "`. Expected type is `" <>
         prettyType expected <> "` while actual type is `" <>
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
  AmbiguousTypeVar cls ->
    "Ambiguity detected while picking an implementation for interface `" <>
    cls <> "`."

data CheckAppendError
  = CheckingSubsumption (Map Text Type) (Map Text Type) Text

printCheckAppendErr :: CheckAppendError -> Text
printCheckAppendErr = \case
  CheckingSubsumption big small field ->
    "While checking field `" <> field <> "`."
