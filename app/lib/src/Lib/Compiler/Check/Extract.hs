{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Check.Extract where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Type

data SourceText = SourceText
  { getSpan :: Span
  , getText :: Text
  }

exSourceText :: SourceAst -> SourceText
exSourceText (span :< (unsafePrj -> (DeclName txt))) =
  SourceText span txt

data ExtractedDecl
  = EDData ExDataDecl
  | EDClass ExClassDecl
  | EDInstance ExInstanceDecl
  | EDType ExTypeDecl
  | EDValue ExValueDecl
  | EDFixity ExFixityDecl

extractDecls :: [SourceAst] -> [ExtractedDecl]
extractDecls = map extractDecl

extractDecl :: SourceAst -> ExtractedDecl
extractDecl (span :< (unsafePrj -> decl)) = case decl of
  DataDecl dDocString
           (exSourceText -> dName)
           (map exSourceText -> dArgs)
           (map goConstr -> dConstrs) ->
    EDData ExDataDecl { dSpan = span, .. }

  ClassDecl cDocString
            (exSourceText *** exSourceText -> cHead)
            (map (exSourceText *** exSourceText) -> cSupers)
            (extractTypeDecls . extractDecls -> cMethods) ->
    EDClass ExClassDecl { cSpan = span, .. }

  InstanceDecl (exSourceText *** hoistCofree unsafePrj -> iHead)
               (map (exSourceText *** hoistCofree unsafePrj) -> iConstraints)
               (extractValueDecls . extractDecls -> iMethods) ->
    EDInstance ExInstanceDecl { iSpan = span, .. }

  TypeDecl tDocString
           (exSourceText -> tName)
           (map (hoistCofree unsafePrj) -> tPolyType) ->
    EDType ExTypeDecl { tSpan = span, .. }

  ValueDecl (exSourceText -> vName) binders e ->
    EDValue ExValueDecl { vSpan = span, .. }
    where vExpr = foldr spanAbs e binders

  FixityDecl (exSourceText -> fAlias)
             (exSourceText -> fOperator)
             fFixity ->
    EDFixity ExFixityDecl { fSpan = span, .. }

  where

    goConstr (docStr, name, args) =
      (docStr, exSourceText name, map (hoistCofree unsafePrj) args)


--------------------------------------------------------------------------------

data ExDataDecl = ExDataDecl
  { dSpan      :: Span
  , dDocString :: Text
  , dName      :: SourceText
  , dArgs      :: [SourceText]
  , dConstrs   :: [(Text, SourceText, [SourceType])]
  }

extractDataDecls :: [ExtractedDecl] -> [ExDataDecl]
extractDataDecls = mapMaybe $ \case
  EDData x -> Just x
  _        -> Nothing

data ExClassDecl = ExClassDecl
  { cSpan      :: Span
  , cDocString :: Text
  , cHead      :: (SourceText, SourceText)
  , cSupers    :: [(SourceText, SourceText)]
  , cMethods   :: [ExTypeDecl]
  }

extractClassDecls :: [ExtractedDecl] -> [ExClassDecl]
extractClassDecls = mapMaybe $ \case
  EDClass x -> Just x
  _         -> Nothing

data ExInstanceDecl = ExInstanceDecl
  { iSpan        :: Span
  , iHead        :: (SourceText, SourceType)
  , iConstraints :: [(SourceText, SourceType)]
  , iMethods     :: [ExValueDecl]
  }

extractInstanceDecls :: [ExtractedDecl] -> [ExInstanceDecl]
extractInstanceDecls = mapMaybe $ \case
  EDInstance x -> Just x
  _            -> Nothing

data ExTypeDecl = ExTypeDecl
  { tSpan      :: Span
  , tDocString :: Text
  , tName      :: SourceText
  , tPolyType  :: SourcePolyType
  }

extractTypeDecls :: [ExtractedDecl] -> [ExTypeDecl]
extractTypeDecls = mapMaybe $ \case
  EDType x -> Just x
  _        -> Nothing

data ExValueDecl = ExValueDecl
  { vSpan :: Span
  , vName :: SourceText
  , vExpr :: SourceAst
  }

extractValueDecls :: [ExtractedDecl] -> [ExValueDecl]
extractValueDecls = mapMaybe $ \case
  EDValue x -> Just x
  _         -> Nothing

data ExFixityDecl = ExFixityDecl
  { fSpan     :: Span
  , fOperator :: SourceText
  , fAlias    :: SourceText
  , fFixity   :: Fixity
  }

extractFixityDecls :: [ExtractedDecl] -> [ExFixityDecl]
extractFixityDecls = mapMaybe $ \case
  EDFixity x -> Just x
  _          -> Nothing
