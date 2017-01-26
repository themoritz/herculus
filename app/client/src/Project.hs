{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |

module Project where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq                (NFData)
import           Control.Lens                   hiding (op)
import           Control.Monad.Except
import           Control.Monad.State            hiding (State)

import           Data.Foldable                  (for_)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Proxy
import           Data.Text                      (Text)

import           GHC.Generics                   (Generic)
import           React.Flux.Addons.Servant      (request)
import           React.Flux.Addons.Servant.Auth (AuthenticateReq)

import           Lib.Api.Rest                   as Api
import           Lib.Api.WebSocket              (Diff)
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Store.Types

--------------------------------------------------------------------------------

data State = State
  { _stateProjectId    :: Id ProjectClient
  , _stateProject      :: ProjectClient
  , _stateCells        :: Map Coords CellContent
  , _stateRowCache     :: Map (Id Table) RowCacheState
  , _stateTables       :: Map (Id Table) TableDesc
  , _stateCurrentTable :: Maybe (Id Table)
  , _stateNewColShow   :: Bool
  } deriving (Show)

data TableDesc = TableDesc
  { _descTable   :: Table
  , _descColumns :: Map (Id Column) Column
  , _descRows    :: Map (Id Row) Row
  } deriving (Show)

mkTableDesc :: Table -> TableDesc
mkTableDesc table = TableDesc table Map.empty Map.empty

type RowCacheState = Map (Id Row) (Map (Id Column) (Column, CellContent))

data Coords = Coords (Id Column) (Id Row)
  deriving (Eq, Ord, Show)

makeLenses ''State
makeLenses ''TableDesc

mkState :: Id ProjectClient
        -> ProjectClient
        -> [Entity Table]
        -> [Entity Column]
        -> [Entity Row]
        -> [Entity Cell]
        -> State
mkState i p tables columns rows cells = execState prepare State
    { _stateProjectId    = i
    , _stateProject      = p
    , _stateCells        = Map.empty
    , _stateRowCache     = Map.empty
    , _stateTables       = Map.empty
    , _stateCurrentTable = case tables of
        []             -> Nothing
        Entity t _ : _ -> Just t
    , _stateNewColShow   = False
    }
  where
    prepare = do
      -- For every table, initialize the table description
      for_ tables $ \(Entity tableId table) ->
        stateTables . at tableId .= Just (mkTableDesc table)
      -- Initialize the cell by coords query
      for_ cells $ \(Entity _ cell) -> do
        let columnId = cell ^. cellColumnId
            rowId = cell ^. cellRowId
        stateCells . at (Coords columnId rowId) .= Just (cell ^. cellContent)
      for_ rows $ \(Entity rowId row) -> do
        let tableId = row ^. rowTableId
        -- Add the row to the table description
        stateTables . at tableId . _Just . descRows . at rowId .= Just row
        -- For every column with the same tableId, set the content of the
        -- row cache.
        for_ columns $ \(Entity columnId column) ->
          when (tableId == column ^. columnTableId) $
            use (stateCells . at (Coords columnId rowId)) >>= \case
              Nothing -> pure ()
              Just content ->
                stateRowCache . at tableId . non Map.empty
                              . at rowId . non Map.empty
                              . at columnId .= Just (column, content)
        -- Add every column to the description of its corresponding table
      for_ columns $ \(Entity columnId column) -> do
        let tableId = column ^. columnTableId
        stateTables . at tableId . _Just
                    . descColumns . at columnId .= Just column

--------------------------------------------------------------------------------

data Action
  = ApplyDiff (Diff Cell) (Diff Column) (Diff Row) (Diff Table)
  | SetName Text
  | RunCommand Command
  -- Tables
  | OpenTable (Id Table)
  -- Table
  | TableToggleNewColumnDialog
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
  apiCall     = DSL . apiCall
  sendWS      = DSL . sendWS
  showMessage = DSL . showMessage
  haltMessage = DSL . haltMessage

run :: (MonadStore m, HasProjectState m)
    => AuthenticateReq Api.SessionProtect -> Action -> m ()
run token action = runDSL (eval token action)

--------------------------------------------------------------------------------

eval :: (MonadStore m, HasProjectState m)
     => AuthenticateReq Api.SessionProtect
     -> Action -> DSL m ()
eval token = \case

  ApplyDiff cellDiff columnDiff rowDiff tableDiff -> do
    -- Tables
    for_ tableDiff $ \(tableId, op, table) -> do
      let tableLens = stateTables . at tableId
          rowCacheLens = stateRowCache . at tableId
      case op of
        Create -> do
          tableLens .= Just (mkTableDesc table)
          rowCacheLens .= Just Map.empty
          -- Go to new table if currently viewing none
          use stateCurrentTable >>= \case
            Just _ -> pure ()
            Nothing -> stateCurrentTable .= Just tableId
        Update -> tableLens . _Just . descTable .= table
        Delete -> do
          tableLens .= Nothing
          rowCacheLens .= Nothing
          -- Select another table if available
          tables <- use stateTables
          let mNextTableId = Map.lookupLT tableId tables
                         <|> Map.lookupGT tableId tables
          case mNextTableId of
            Nothing ->
              stateCurrentTable .= Nothing
            Just (nextTableId, _) ->
              stateCurrentTable .= Just nextTableId
    -- Cells
    for_ cellDiff $ \(_, op, cell) -> do
      let columnId = cell ^. cellColumnId
          rowId = cell ^. cellRowId
          tableId = cell ^. cellTableId
          coords = Coords columnId rowId
          content = cell ^. cellContent
          cellLens = stateCells . at coords
      case op of
        Create -> cellLens .= Just content
        Update -> do
          cellLens .= Just content
          -- Update of the row cache when cells are created / deleted is
          -- handled by the rows / columns
          stateRowCache . at tableId . _Just
                        . at rowId . _Just
                        . at columnId . _Just
                        . _2 .= content
        Delete -> cellLens .= Nothing
    -- Rows
    for_ rowDiff $ \(rowId, op, row) -> do
      let tableId = row ^. rowTableId
          tablesLens = stateTables . at tableId . _Just . descRows . at rowId
          rowCacheLens = stateRowCache . at tableId . non Map.empty . at rowId
      case op of
        Create -> do
          tablesLens .= Just row
          columns <- use (stateTables . at tableId . _Just . descColumns)
          for_ (Map.toList columns) $ \(columnId, column) ->
            use (stateCells . at (Coords columnId rowId)) >>= \case
              Nothing -> pure ()
              Just content ->
                rowCacheLens . non Map.empty . at columnId .=
                  Just (column, content)
        Update -> tablesLens .= Just row
        Delete -> do
          tablesLens .= Nothing
          rowCacheLens .= Nothing
    -- Columns
    for_ columnDiff $ \(columnId, op, column) -> do
      let tableId = column ^. columnTableId
          tablesLens = stateTables . at tableId . _Just
                                   . descColumns . at columnId
          rowCacheLens r = stateRowCache . at tableId . non Map.empty
                                         . at r . non Map.empty . at columnId
      rows <- use (stateTables . at tableId . _Just . descRows)
      case op of
        Create -> do
          tablesLens .= Just column
          for_ (Map.keys rows) $ \rowId ->
            use (stateCells . at (Coords columnId rowId)) >>= \case
              Nothing -> pure ()
              Just content ->
                rowCacheLens rowId .= Just (column, content)
        Update -> do
          tablesLens .= Just column
          for_ (Map.keys rows) $ \rowId ->
            rowCacheLens rowId . _Just . _1 .= column
        Delete -> do
          tablesLens .= Nothing
          for_ (Map.keys rows) $ \rowId ->
            rowCacheLens rowId .= Nothing

  SetName name -> do
    projectId <- use stateProjectId
    apiCall $ request api (Proxy :: Proxy Api.ProjectSetName)
                          token projectId name
    stateProject . projectClientName .= name

  RunCommand cmd -> do
    projectId <- use stateProjectId
    apiCall $ request api (Proxy :: Proxy Api.ProjectRunCommand)
                          token projectId cmd

  OpenTable tableId ->
    stateCurrentTable .= Just tableId

  TableToggleNewColumnDialog -> stateNewColShow %= not
