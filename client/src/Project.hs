{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |

module Project where

import           Control.Applicative            ((<|>))
import           Control.Arrow                  (second)
import           Control.DeepSeq                (NFData)
import           Control.Lens                   hiding (op)
import           Control.Monad.Except
import           Control.Monad.State            hiding (State)

import           Data.Foldable                  (foldl', for_)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Proxy
import qualified Data.Set                       as Set
import           Data.Text                      (Text)

import           GHC.Generics                   (Generic)
import           React.Flux.Addons.Servant      (request)
import           React.Flux.Addons.Servant.Auth (AuthenticateReq)

import           Lib.Api.Rest                   as Api
import           Lib.Api.WebSocket              (Diff)
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies.Types   (tablesOfTypeDeps)
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Store.Types

--------------------------------------------------------------------------------

-- TODO: maybe put tableId and tables one level deeper into project?
data State = State
  { _stateProjectId  :: Id ProjectClient
  , _stateProject    :: ProjectClient
  , _stateCacheRows  :: Map (Id Table) RowCacheState
  , _stateTableId    :: Maybe (Id Table)
  , _stateColumns    :: Map (Id Column) Column
  , _stateTables     :: Map (Id Table) Table
  , _stateCells      :: Map Coords CellContent
  , _stateRows       :: Map (Id Row) Row
  , _stateNewColShow :: Bool
  } deriving (Show)

mkState :: Id ProjectClient -> ProjectClient -> State
mkState i p = State
  { _stateProjectId  = i
  , _stateProject    = p
  , _stateCacheRows  = Map.empty
  , _stateTableId    = Nothing
  , _stateColumns    = Map.empty
  , _stateTables     = Map.empty
  , _stateCells      = Map.empty
  , _stateRows       = Map.empty
  , _stateNewColShow = False
  }

type RowCacheState = Map (Id Row) (Map (Id Column) (Column, CellContent))

data Coords = Coords (Id Column) (Id Row)
  deriving (Eq, Ord, Show)

makeLenses ''State

--------------------------------------------------------------------------------

data Action
  = ApplyDiff (Diff Cell) (Diff Column) (Diff Row) (Diff Table)
  | SetName Text
  -- Tables
  | CreateTable Text
  | LoadTable (Id Table)
  -- Table
  | TableToggleNewColumnDialog
  | TableCreateDataCol
  | TableCreateReportCol
  | TableRenameColumn (Id Column) Text
  | TableUpdateDataCol (Id Column) DataType IsDerived Text
  | TableUpdateReportCol (Id Column) Text ReportFormat (Maybe ReportLanguage)
  | TableDeleteColumn (Id Column)
  | TableAddRow
  | TableDeleteRow (Id Row)
  | TableSetName (Id Table) Text
  | TableDelete (Id Table)
  -- Cell
  | CellSetValue (Id Column) (Id Row) Value
  deriving (Generic, Show)

instance NFData Action

--------------------------------------------------------------------------------

newtype DSL m a = DSL
  { runDSL :: m a
  } deriving ( Functor
             , Applicative
             , Monad
             )

class Monad m => HasProjectState m where
  projectState :: (State -> (a, State)) -> m a

instance HasProjectState m => MonadState State (DSL m) where
  state = DSL . projectState

instance MonadStore m => MonadStore (DSL m) where
  apiCall = DSL . apiCall

run :: (MonadStore m, HasProjectState m)
    => AuthenticateReq Api.SessionProtect -> Action -> m ()
run token action = runDSL (evalProject token action)

--------------------------------------------------------------------------------

evalProject :: (MonadStore m, HasProjectState m)
            => AuthenticateReq Api.SessionProtect
            -> Action -> DSL m ()
evalProject token = \case

  ApplyDiff cellDiff columnDiff rowDiff tableDiff -> do
    mTableId <- use stateTableId
    -- Cells, columns and rows are only updated if we are currently
    -- viewing a table.
    for_ mTableId $ \tableId -> do
      -- Cells
      for_ cellDiff $ \(_, op, cell) ->
        when (cell ^. cellTableId == tableId) $ do
          let coords = Coords (cell ^. cellColumnId) (cell ^. cellRowId)
              content = cell ^. cellContent
          stateCells . at coords .= case op of
            Create -> Just content
            Update -> Just content
            Delete -> Nothing
      -- Columns
      for_ columnDiff $ \(columnId, op, column) ->
        when (column ^. columnTableId == tableId) $ do
          stateColumns . at columnId .= case op of
            Create -> Just column
            Update -> Just column
            Delete -> Nothing
          when (op == Delete) $
            stateCells %= Map.filterWithKey
                            (\(Coords c _) _ -> c /= columnId)
      -- Rows
      for_ rowDiff $ \(rowId, op, row) ->
        when (row ^. rowTableId == tableId) $
          stateRows . at rowId .= case op of
            Create -> Just row
            Update -> Just row
            Delete -> Nothing
          -- TODO: On delete: delete all cells of the row
    -- Tables
    for_ tableDiff $ \(tableId, op, table) -> do
      stateTables . at tableId .= case op of
        Create -> Just table
        Update -> Just table
        Delete -> Nothing
      -- Select another table if available
      when (op == Delete) $ do
        stateColumns .= Map.empty
        stateRows .= Map.empty
        stateCells .= Map.empty
        tables <- use stateTables
        let mNextTableId = Map.lookupLT tableId tables
                       <|> Map.lookupGT tableId tables
        case mNextTableId of
          Nothing -> pure ()
          Just (nextTableId, _) ->
            evalProject token $ LoadTable nextTableId

  SetName name -> do
    projectId <- use stateProjectId
    apiCall $ request api (Proxy :: Proxy Api.ProjectSetName)
                          token projectId name
    stateProject . projectClientName .= name

  CreateTable name -> do
    projectId <- use stateProjectId
    apiCall $ request api (Proxy :: Proxy Api.TableCreate)
      token projectId name

  LoadTable tableId -> do
    (cols, rows, cells) <-
      apiCall $ request api (Proxy :: Proxy Api.TableGetWhole) token tableId

    let fillEntries entries m = foldl' acc m $ map toCoords entries
          where
            toCoords (colId, recId, val) = (Coords colId recId, val)
            acc m' (c, v) = Map.insert c v m'
    stateColumns .= Map.fromList (map entityToTuple cols)
    stateRows    .= Map.fromList (map entityToTuple rows)
    stateCells   .= fillEntries cells Map.empty
    stateTableId .= Just tableId

    -- For every column that has a reference to a table in its type, fill
    -- the rowcache for that table.
    for_ cols $ \(Entity _ column) ->
      case column ^? columnKind . _ColumnData . dataColType of
        Nothing -> pure ()
        Just typ -> do
          let tables = Set.toList $ tablesOfTypeDeps $ getTypeDependencies typ
          for_ tables $ \tableId' ->
            use (stateCacheRows . at tableId') >>= \case
              -- Cache not yet initialized: no key in map => Nothing
              Just _  -> pure ()
              Nothing -> do
                records <- apiCall $ request api
                  (Proxy :: Proxy Api.RowListWithData) token tableId'
                let toCol (Entity c col, content) = (c, (col, content))
                    recMaps = map (second $ Map.fromList . map toCol) records
                stateCacheRows . at tableId' .= Just (Map.fromList recMaps)

  TableToggleNewColumnDialog -> stateNewColShow %= not

  TableCreateDataCol -> use stateTableId >>= \case
    Nothing -> pure ()
    Just tableId ->
      apiCall $ request api (Proxy :: Proxy Api.DataColCreate) token tableId

  TableCreateReportCol -> use stateTableId >>= \case
    Nothing -> pure ()
    Just tableId ->
      apiCall $ request api (Proxy :: Proxy Api.ReportColCreate) token tableId

  TableRenameColumn columnId name ->
    apiCall $ request api (Proxy :: Proxy Api.ColumnSetName) token columnId name

  TableUpdateDataCol columnId dt inpTyp src ->
    apiCall $ request api (Proxy :: Proxy Api.DataColUpdate)
                          token columnId (dt, inpTyp, src)

  TableUpdateReportCol columnId template format lang ->
    apiCall $ request api (Proxy :: Proxy Api.ReportColUpdate)
                          token columnId (template, format, lang)

  TableDeleteColumn columnId ->
    apiCall $ request api (Proxy :: Proxy Api.ColumnDelete) token columnId

  TableAddRow -> use stateTableId >>= \case
    Nothing -> pure ()
    Just tableId ->
      apiCall $ request api (Proxy :: Proxy Api.RowCreate) token tableId

  TableDeleteRow rowId ->
    apiCall $ request api (Proxy :: Proxy Api.RowDelete) token rowId

  TableSetName tableId name ->
    apiCall $ request api (Proxy :: Proxy Api.TableSetName) token tableId name

  TableDelete tableId ->
    apiCall $ request api (Proxy :: Proxy Api.TableDelete) token tableId

  CellSetValue c r val ->
    apiCall $ request api (Proxy :: Proxy Api.CellSet) token c r val
