{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- |

module Main where

import           Control.Lens

import           Data.Proxy
import qualified Data.Set                                  as Set

import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeParameters
import           Servant.PureScript

import           Lib.Api.Rest
import           Lib.Model
import           Lib.Model.Auth
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table

data Bridge

bridgeProxy :: Proxy Bridge
bridgeProxy = Proxy

bridge :: BridgePart
bridge =
      defaultBridge
  <|> (typeName ^== "CExpr" >> pure psString)
  <|> (typeName ^== "CTemplate" >> pure psString)
  <|> (typeName ^== "Number" >> pure psNumber)
  <|> (typeName ^== "Time" >> pure psString)
  <|> (typeName ^== "Base64" >> pure psString)
  <|> (typeName ^== "Base64Url" >> pure psString)
  <|> projectBridge
  <|> utcTimeBridge

projectBridge :: BridgePart
projectBridge = do
  typeName ^== "Project"
  pure $ TypeInfo "" "Lib.Model.Project" "ProjectClient" []

utcTimeBridge :: BridgePart
utcTimeBridge = do
  typeName ^== "UTCTime"
  pure $ TypeInfo "purescript-datetime" "Data.Date" "Date" []

instance HasBridge Bridge where
  languageBridge _ = buildBridge bridge

types :: [SumType 'Haskell]
types =
  -- Lib.Api.Rest
  [ mkSumType (Proxy @Command)
  -- Lib.Model
  , mkSumType (Proxy @(Entity A))
  -- Lib.Model.Auth
  , mkSumType (Proxy @ChangePwdData)
  , mkSumType (Proxy @ChangePwdResponse)
  , mkSumType (Proxy @GetUserInfoResponse)
  , mkSumType (Proxy @UserInfo)
  , mkSumType (Proxy @User)
  , mkSumType (Proxy @Email)
  , mkSumType (Proxy @LoginData)
  , mkSumType (Proxy @LoginResponse)
  , mkSumType (Proxy @SignupData)
  , mkSumType (Proxy @SignupResponse)
  -- Lib.Model.Cell
  , mkSumType (Proxy @Cell)
  , mkSumType (Proxy @CellContent)
  , mkSumType (Proxy @Value)
  -- Lib.Model.Column
  , mkSumType (Proxy @Column)
  , mkSumType (Proxy @ColumnKind)
  , mkSumType (Proxy @DataCol)
  , mkSumType (Proxy @ReportCol)
  , mkSumType (Proxy @ReportLanguage)
  , mkSumType (Proxy @ReportFormat)
  , mkSumType (Proxy @DataType)
  , mkSumType (Proxy @(CompileResult A))
  , mkSumType (Proxy @IsDerived)
  -- Lib.Model.Project
  , mkSumType (Proxy @ProjectClient)
  -- Lib.Model.Row
  , mkSumType (Proxy @Row)
  -- Lib.Model.Table
  , mkSumType (Proxy @Table)
  ]

main :: IO ()
main = do
  writePSTypes "../psc-client/src" (buildBridge bridge) types
  let
    settings m = defaultSettings
      & apiModuleName .~ m
      & readerParams .~ Set.fromList
          [ "Authorization"
          , "baseURL"
          ]
    writeApiToModule m = writeAPIModuleWithSettings (settings m)
  writeApiToModule "Api.Rest" "../psc-client/src" bridgeProxy (Proxy @Routes')
