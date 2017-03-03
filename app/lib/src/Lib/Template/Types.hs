{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Lib.Template.Types where

import           Control.Monad.Except

import           Data.Aeson
import           Data.Monoid
import           Data.Text                    (Text)

import           GHC.Generics

import           Lib.Compiler.Types
import           Lib.Model.Dependencies.Types
import           Lib.Types

newtype PTemplate = PTemplate [PTplExpr]
  deriving (Show)

data PTplExpr
  = PTplText Text
  | PTplFor Name PExpr PTemplate
  | PTplIf PExpr PTemplate PTemplate
  | PTplShow PExpr
  deriving (Show)

newtype TTemplate = TTemplate [TTplExpr]

data TTplExpr
  = TTplText Text
  | TTplFor Name TExpr TTemplate
  | TTplIf TExpr TTemplate TTemplate
  | TTplShow TExpr

newtype CTemplate = CTemplate [CTplExpr]
  deriving (Eq, Show, Generic)

instance ToJSON CTemplate
instance FromJSON CTemplate

-- Compiled template using core expressions
data CTplExpr
  = CTplText Text
  | CTplFor Name CExpr CTemplate
  | CTplIf CExpr CTemplate CTemplate
  | CTplShow CExpr
  deriving (Eq, Show, Generic)

instance ToJSON CTplExpr
instance FromJSON CTplExpr

collectTplCodeDependencies :: CTemplate -> CodeDependencies
collectTplCodeDependencies = go
  where go (CTemplate tpls) = mconcat $ map goOne tpls
        goOne e' = case e' of
          CTplText _           -> mempty
          CTplFor _ e body     -> collectCodeDependencies e <> go body
          CTplIf e then' else' -> collectCodeDependencies e <> go then' <> go else'
          CTplShow e           -> collectCodeDependencies e

toCoreTpl :: MonadError TypeError m => TTemplate -> m CTemplate
toCoreTpl (TTemplate tpls) = CTemplate <$> mapM toCore tpls
  where
    toCore = \case
      TTplText t      -> pure $ CTplText t
      TTplFor x e tpl -> CTplFor x <$> toCoreExpr e <*> toCoreTpl tpl
      TTplIf c t e    -> CTplIf <$> toCoreExpr c <*> toCoreTpl t <*> toCoreTpl e
      TTplShow e      -> CTplShow <$> toCoreExpr e
