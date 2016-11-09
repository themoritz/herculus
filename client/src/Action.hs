{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- |

module Action where

import           Control.DeepSeq                (NFData)
import           Data.Map                       (Map)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)
import           React.Flux.Addons.Servant      (ApiRequestConfig (..),
                                                 HandleResponse,
                                                 RequestTimeout (NoTimeout))
import           React.Flux.Addons.Servant.Auth (AuthClientData,
                                                 AuthenticateReq,
                                                 mkAuthenticateReq)

import qualified Config
import           Lib.Api.Rest                   as Api
import           Lib.Api.WebSocket              (WsUpMessage)
import           Lib.Model                      (Entity)
import           Lib.Model.Auth                 (LoginData, SessionKey,
                                                 SignupData, UserInfo)
import           Lib.Model.Cell                 (Cell, CellContent, Value)
import           Lib.Model.Column               (Column)
import           Lib.Model.Project              (Project)
import           Lib.Model.Record               (Record)
import           Lib.Model.Table                (Table)
import           Lib.Types                      (Id)
import           Lib.Util.Base64                (unBase64)

import qualified Action.Column                  as Column
import qualified Action.Message                 as Message
import qualified Action.RecordCache             as RecordCache

type instance AuthClientData Api.SessionProtect = SessionKey

mkAuthHeader :: AuthClientData Api.SessionProtect -> (Text, Text)
mkAuthHeader sessionKey =
  (Api.sessionParamStr, Text.decodeUtf8 $ unBase64 sessionKey)

session :: SessionKey -> AuthenticateReq Api.SessionProtect
session key = mkAuthenticateReq key mkAuthHeader

api :: ApiRequestConfig Routes
api = ApiRequestConfig Config.apiUrl NoTimeout

type Callback = forall b. ((b -> [Action]) -> HandleResponse b)

type TableCache = Map (Id Table) Text

data Action
  -- Global
  = MessageAction Message.Action
  | GlobalInit Text -- WebSocket URL
  | GlobalSendWebSocket WsUpMessage
  -- Session
  | ToSignupForm
  | Signup SignupData
  | Login LoginData
  | LoggedIn UserInfo
  | Logout

  | RecordCacheAction (Id Table) RecordCache.Action
  -- Projects
  | ProjectsSet [Entity Project]
  | ProjectsCreate Project
  | ProjectsAdd (Entity Project)
  | ProjectsLoadProject (Id Project)
  | ProjectDelete (Id Project)
  -- Project
  | ProjectSetName (Id Project) Text
  -- Tables
  | TablesSet [Entity Table]
  | TablesCreate Table
  | TablesAdd (Entity Table)
  | TablesLoadTable (Id Table)
  -- Table
  | TableSet ([Entity Column], [Entity Record], [(Id Column, Id Record, CellContent)])
  | TableUpdateCells [Cell]
  | TableUpdateColumns [Entity Column]
  | TableAddColumn Column
  | TableAddColumnDone (Entity Column, [Entity Cell])
  | TableDeleteColumn (Id Column)
  | TableAddRecord
  | TableAddRecordDone (Entity Record, [Entity Cell])
  | TableDeleteRecord (Id Record)
  | TableSetName (Id Table) Text
  | TableDelete (Id Table)
  -- column config: table cache
  | GetTableCache
  | SetTableCache          TableCache
  -- Cell
  | CellSetValue (Id Column) (Id Record) Value
  | ColumnAction (Id Column) Column.Action
  deriving (Typeable, Generic, NFData)
