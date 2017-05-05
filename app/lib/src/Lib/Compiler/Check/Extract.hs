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

--------------------------------------------------------------------------------

data ExDataDecl = ExDataDecl
  { dSpan    :: Span
  , dName    :: SourceText
  , dArgs    :: [SourceText]
  , dConstrs :: [(SourceText, [SourceType])]
  }

extractDataDecls :: [SourceAst] -> [ExDataDecl]
extractDataDecls = mapMaybe $ \(dSpan :< (unsafePrj -> decl)) -> case decl of
  DataDecl (exSourceText -> dName)
           (map exSourceText -> dArgs)
           (map (exSourceText *** map (hoistCofree unsafePrj)) -> dConstrs) ->
    Just $ ExDataDecl {..}
  _                            -> Nothing

data ExClassDecl = ExClassDecl
  { cSpan    :: Span
  , cHead    :: (SourceText, SourceText)
  , cSupers  :: [(SourceText, SourceText)]
  , cMethods :: [ExTypeDecl]
  }

extractClassDecls :: [SourceAst] -> [ExClassDecl]
extractClassDecls = mapMaybe $ \(cSpan :< (unsafePrj -> decl)) -> case decl of
  ClassDecl (exSourceText *** exSourceText -> cHead)
            (map (exSourceText *** exSourceText) -> cSupers)
            (extractTypeDecls -> cMethods) ->
    Just $ ExClassDecl {..}
  _                       -> Nothing

data ExInstanceDecl = ExInstanceDecl
  { iSpan        :: Span
  , iHead        :: (SourceText, SourceType)
  , iConstraints :: [(SourceText, SourceType)]
  , iMethods     :: [ExValueDecl]
  }

extractInstanceDecls :: [SourceAst] -> [ExInstanceDecl]
extractInstanceDecls = mapMaybe $ \(iSpan :< (unsafePrj -> decl)) -> case decl of
  InstanceDecl (exSourceText *** hoistCofree unsafePrj -> iHead)
               (map (exSourceText *** hoistCofree unsafePrj) -> iConstraints)
               (extractValueDecls -> iMethods) ->
    Just $ ExInstanceDecl {..}
  _                         -> Nothing

data ExTypeDecl = ExTypeDecl
  { tSpan     :: Span
  , tName     :: SourceText
  , tPolyType :: SourcePolyType
  }

extractTypeDecls :: [SourceAst] -> [ExTypeDecl]
extractTypeDecls = mapMaybe $ \(tSpan :< (unsafePrj -> decl)) -> case decl of
  TypeDecl (exSourceText -> tName)
           (map (hoistCofree unsafePrj) -> tPolyType) ->
    Just $ ExTypeDecl {..}
  _                   -> Nothing

data ExValueDecl = ExValueDecl
  { vSpan :: Span
  , vName :: SourceText
  , vExpr :: SourceAst
  }

extractValueDecls :: [SourceAst] -> [ExValueDecl]
extractValueDecls = mapMaybe $ \(vSpan :< (unsafePrj -> decl)) -> case decl of
  ValueDecl (exSourceText -> vName) binders e -> Just $ ExValueDecl {..}
    where vExpr = foldr spanAbs e binders
  _                         -> Nothing

data ExFixityDecl = ExFixityDecl
  { fSpan     :: Span
  , fOperator :: SourceText
  , fAlias    :: SourceText
  , fFixity   :: Fixity
  }

extractFixityDecls :: [SourceAst] -> [ExFixityDecl]
extractFixityDecls = mapMaybe $ \(fSpan :< (unsafePrj -> decl)) -> case decl of
  FixityDecl (exSourceText -> fAlias)
             (exSourceText -> fOperator)
             fFixity ->
    Just $ ExFixityDecl {..}
  _                                   -> Nothing
