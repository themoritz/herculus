{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Lib.Template.Core where

import           Lib.Prelude

import           Data.Aeson
import           Data.Functor.Foldable

import           Lib.Model.Dependencies.Types

import           Lib.Compiler.Core            as C
import qualified Lib.Template.AST             as A

data TplChunk
  = TplText Text
  | TplFor Binder Expr [TplChunk]
  | TplIf Expr [TplChunk] [TplChunk]
  | TplPrint Expr
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

collectTplCodeDependencies :: [TplChunk] -> CodeDependencies
collectTplCodeDependencies = mconcat . map go
  where
  go :: TplChunk -> CodeDependencies
  go = \case
    TplText _ ->
      mempty
    TplFor _ e body ->
      C.collectCodeDependencies e <> mconcat (map go body)
    TplIf e th el ->
      C.collectCodeDependencies e <> mconcat (map go th) <> mconcat (map go el)
    TplPrint e ->
      C.collectCodeDependencies e

toCore :: [A.TplCompiled] -> [TplChunk]
toCore = map go
  where
  go :: A.TplCompiled -> TplChunk
  go = cata $ \case
    A.TplText t -> TplText t
    A.TplFor b e body -> TplFor (C.binderToCore b) (C.toCore e) body
    A.TplIf e th el -> TplIf (C.toCore e) th el
    A.TplPrint e -> TplPrint (C.toCore e)
