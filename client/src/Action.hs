{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- |

module Action where

import           Control.DeepSeq                (NFData)
import           Data.Map                       (Map)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)
import           React.Flux                     (ReactStore,
                                                 SomeStoreAction (..),
                                                 StoreData (StoreAction))
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
import           Lib.Model.Auth                 (LoginData, SessionKey)
import           Lib.Model.Cell                 (Cell, CellContent, Value)
import           Lib.Model.Column               (Column)
import           Lib.Model.Project              (Project)
import           Lib.Model.Record               (Record)
import           Lib.Model.Table                (Table)
import           Lib.Types                      (Id)
import           Lib.Util.Base64                (unBase64)

import qualified Action.Column.Types            as Column
import qualified Action.RecordCache.Types       as RecordCache

type instance AuthClientData Api.SessionProtect = Maybe SessionKey

mkAuthHeader :: AuthClientData Api.SessionProtect -> (Text, Text)
mkAuthHeader Nothing = (Api.sessionParamStr, "")
mkAuthHeader (Just sessionKey) =
  (Api.sessionParamStr, Text.decodeUtf8 $ unBase64 sessionKey)

session :: Maybe SessionKey -> AuthenticateReq Api.SessionProtect
session key = mkAuthenticateReq key mkAuthHeader

api :: ApiRequestConfig Routes
api = ApiRequestConfig Config.apiUrl NoTimeout

dispatch :: (StoreData s, NFData (StoreAction s))
         => ReactStore s
         -> StoreAction s
         -> [SomeStoreAction]
dispatch store a = [SomeStoreAction store a]

type Callback s = forall a. (a -> [StoreAction s]) -> HandleResponse a

mkCallback :: (StoreData s, NFData (StoreAction s))
           => ReactStore s
           -> (Text -> StoreAction s)
           -> (a -> [StoreAction s])
           -> HandleResponse a
mkCallback store cbFail cbSuccess = pure . \case
  Left  (_, e) -> dispatch store $ cbFail $ Text.pack e
  Right x      -> SomeStoreAction store <$> cbSuccess x

type TableCache = Map (Id Table) Text

data Action
  -- Global
  = GlobalSetError Text
  | GlobalInit Text -- WebSocket URL
  | GlobalSendWebSocket WsUpMessage
  -- Session
  | Login LoginData
  | LoggedIn SessionKey
  | Logout
  | LoggedOut

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
  -- Column
  | GetReportFormatPlain (Id Column) (Id Record)
  | GetReportFormatPDF (Id Column) (Id Record)
  | GetReportFormatHTML (Id Column) (Id Record)
  -- column config: table cache
  | GetTableCache          (Maybe SessionKey)
  | SetTableCache          TableCache
  -- Cell
  | CellSetValue (Id Column) (Id Record) Value

  | ColumnAction (Id Column) Column.Action
  deriving (Typeable, Generic, NFData)
