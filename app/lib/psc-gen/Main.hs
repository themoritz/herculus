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
import           Data.Text                                 (Text)

import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeParameters
import           Servant.PureScript

import           Lib.Api.Rest
import           Lib.Api.Schema.Auth
import           Lib.Api.Schema.Column
import           Lib.Api.Schema.Project
import           Lib.Model
import qualified Lib.Model.Auth                            as M
import qualified Lib.Model.Cell                            as M
import qualified Lib.Model.Column                          as M
import qualified Lib.Model.Row                             as M
import qualified Lib.Model.Table                           as M

data Bridge

bridgeProxy :: Proxy Bridge
bridgeProxy = Proxy

bridge :: BridgePart
bridge =
      defaultBridge
  <|> (typeName ^== "Number" >> pure psNumber)
  <|> (typeName ^== "Time" >> pure psString)
  <|> (typeName ^== "Base64" >> pure psString)
  <|> (typeName ^== "Base64Url" >> pure psString)
  <|> (idBridge "Lib.Model.Project" "Project" "ProjectTag")
  <|> (idBridge "Lib.Model.Column" "Column" "ColumnTag")
  <|> utcTimeBridge

idBridge :: Text -> Text -> Text -> BridgePart
idBridge modul name tag = do
  typeModule ^== modul
  typeName ^== name
  pure $ TypeInfo "" "Lib.Types" tag []

utcTimeBridge :: BridgePart
utcTimeBridge = do
  typeName ^== "UTCTime"
  pure $ TypeInfo "purescript-datetime" "Data.Date" "Date" []

instance HasBridge Bridge where
  languageBridge _ = buildBridge bridge

types :: [SumType 'Haskell]
types =
  -- Lib.Model
  [ mkSumType (Proxy @(Entity A))
  -- Lib.Api.Schema.Auth
  , mkSumType (Proxy @ChangePwdData)
  , mkSumType (Proxy @ChangePwdResponse)
  , mkSumType (Proxy @GetUserInfoResponse)
  , mkSumType (Proxy @UserInfo)
  , mkSumType (Proxy @LoginData)
  , mkSumType (Proxy @LoginResponse)
  , mkSumType (Proxy @SignupData)
  , mkSumType (Proxy @SignupResponse)
  -- Lib.Model.Auth
  , mkSumType (Proxy @M.User)
  , mkSumType (Proxy @M.Email)
  -- Lib.Model.Cell
  , mkSumType (Proxy @M.Cell)
  , mkSumType (Proxy @M.CellContent)
  , mkSumType (Proxy @M.Value)
  -- Lib.Api.Schema.Column
  , mkSumType (Proxy @Column)
  , mkSumType (Proxy @ColumnKind)
  , mkSumType (Proxy @DataCol)
  , mkSumType (Proxy @ReportCol)
  , mkSumType (Proxy @CompileStatus)
  -- Lib.Model.Column
  , mkSumType (Proxy @M.ReportLanguage)
  , mkSumType (Proxy @M.ReportFormat)
  , mkSumType (Proxy @M.DataType)
  , mkSumType (Proxy @(M.CompileResult A))
  , mkSumType (Proxy @M.IsDerived)
  -- Lib.Api.Schema.Project
  , mkSumType (Proxy @Command)
  , mkSumType (Proxy @Project)
  , mkSumType (Proxy @ProjectData)
  -- Lib.Model.Row
  , mkSumType (Proxy @M.Row)
  -- Lib.Model.Table
  , mkSumType (Proxy @M.Table)
  ]

main :: IO ()
main = do
  writePSTypes "../psc-client/src" (buildBridge bridge) types
  let
    settings m = defaultSettings
      & apiModuleName .~ m
      & readerParams .~ Set.fromList
          -- [ "Authorization"
          [ "baseURL"
          ]
    writeApiToModule m = writeAPIModuleWithSettings (settings m)
  writeApiToModule "Lib.Api.Rest" "../psc-client/src" bridgeProxy (Proxy @Routes')
