{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Check.Extract where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Type

data ExDataDecl = ExDataDecl
  { dSpan    :: Span
  , dName    :: Text
  , dArgs    :: [Text]
  , dConstrs :: [(Text, [SourceType])]
  }

extractDataDecls :: [SourceAst] -> [ExDataDecl]
extractDataDecls = mapMaybe $ \(dSpan :< (unsafePrj -> decl)) -> case decl of
  DataDecl dName dArgs constrs -> Just $ ExDataDecl {..}
    where dConstrs = map (id *** map (hoistCofree unsafePrj)) constrs
  _                            -> Nothing

data ExClassDecl = ExClassDecl
  { cSpan    :: Span
  , cHead    :: (Text, Text)
  , cSupers  :: [(Text, Text)]
  , cMethods :: [ExTypeDecl]
  }

extractClassDecls :: [SourceAst] -> [ExClassDecl]
extractClassDecls = mapMaybe $ \(cSpan :< (unsafePrj -> decl)) -> case decl of
  ClassDecl cHead cSupers methods -> Just $ ExClassDecl {..}
    where cMethods = extractTypeDecls methods
  _                       -> Nothing

data ExInstanceDecl = ExInstanceDecl
  { iSpan        :: Span
  , iHead        :: SourceConstraint
  , iConstraints :: [SourceConstraint]
  , iMethods     :: [ExValueDecl]
  }

extractInstanceDecls :: [SourceAst] -> [ExInstanceDecl]
extractInstanceDecls = mapMaybe $ \(iSpan :< (unsafePrj -> decl)) -> case decl of
  InstanceDecl h cs methods -> Just $ ExInstanceDecl {..}
    where iHead = map (hoistCofree unsafePrj) h
          iConstraints = map (map (hoistCofree unsafePrj)) cs
          iMethods = extractValueDecls methods
  _                         -> Nothing

data ExTypeDecl = ExTypeDecl
  { tSpan     :: Span
  , tName     :: Text
  , tPolyType :: SourcePolyType
  }

extractTypeDecls :: [SourceAst] -> [ExTypeDecl]
extractTypeDecls = mapMaybe $ \(tSpan :< (unsafePrj -> decl)) -> case decl of
  TypeDecl tName poly -> Just $ ExTypeDecl {..}
    where tPolyType = map (hoistCofree unsafePrj) poly
  _                  -> Nothing

data ExValueDecl = ExValueDecl
  { vSpan :: Span
  , vName :: Text
  , vExpr :: SourceAst
  }

extractValueDecls :: [SourceAst] -> [ExValueDecl]
extractValueDecls = mapMaybe $ \(vSpan :< (unsafePrj -> decl)) -> case decl of
  ValueDecl vName binders e -> Just $ ExValueDecl {..}
    where vExpr = foldr spanAbs e binders
  _                           -> Nothing
