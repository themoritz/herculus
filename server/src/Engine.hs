{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
-- |

module Engine
  ( Command (..)
  , runCommand
  ) where

import           Control.Lens                 hiding (op)
import           Control.Monad.Except

import           Data.Foldable                (for_)
import qualified Data.Map                     as Map
import           Data.Maybe                   (mapMaybe)
import           Data.Text                    (Text)

import           Lib.Api.WebSocket
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Engine.Compile
import           Engine.Monad
import           Engine.Propagate
import           Engine.Util
import           Monads

--------------------------------------------------------------------------------

-- | All the critical commands that should be atomic, undoable, replayable etc.
-- within a project.
data Command
  = CmdTableCreate (Id Project) Text
  | CmdTableSetName (Id Table) Text
  | CmdTableDelete (Id Table)
  | CmdDataColCreate (Id Table)
  | CmdDataColUpdate (Id Column) DataType IsDerived Text
  | CmdReportColCreate (Id Table)
  | CmdReportColUpdate (Id Column) Text ReportFormat (Maybe ReportLanguage)
  | CmdColumnSetName (Id Column) Text
  | CmdColumnDelete (Id Column)
  | CmdRowCreate (Id Table)
  | CmdRowDelete (Id Row)
  | CmdCellSet (Id Column) (Id Row) Value

projectOfCommand :: MonadHexl m => Command -> m (Id Project)
projectOfCommand = \case
    CmdTableCreate p _         -> pure p
    CmdTableSetName t _        -> ofTable t
    CmdTableDelete t           -> ofTable t
    CmdDataColCreate t         -> ofTable t
    CmdDataColUpdate c _ _ _   -> ofColumn c
    CmdReportColCreate t       -> ofTable t
    CmdReportColUpdate c _ _ _ -> ofColumn c
    CmdColumnSetName c _       -> ofColumn c
    CmdColumnDelete c          -> ofColumn c
    CmdRowCreate t             -> ofTable t
    CmdRowDelete r             -> ofRow r
    CmdCellSet c _ _           -> ofColumn c
  where
    ofTable t = _tableProjectId <$> getById' t
    ofColumn c = _columnTableId <$> getById' c >>= ofTable
    ofRow r = _rowTableId <$> getById' r >>= ofTable

--------------------------------------------------------------------------------

runCommand :: MonadHexl m => Command -> m ()
runCommand cmd = do
  projectId <- projectOfCommand cmd
  graph <- _projectDependencyGraph <$> getById' projectId
  -- Run command in engine, compile, and propagate
  (_ , state) <- runEngineT graph $ do
    executeCommand cmd
    getCompileTargets >>= mapM_ compileColumn
    propagate
  -- Commit changes
  let Store{..} = state ^. engineStore
  commit _storeCells
  commit _storeColumns
  commit _storeRows
  commit _storeTables
  update projectId (projectDependencyGraph .~ (state ^. engineGraph))
  -- Send changes to clients
  sendWS $ WsDownProjectDiff (filterChanges _storeCells)
                             (filterChanges _storeColumns)
                             (filterChanges _storeRows)
                             (filterChanges _storeTables)

filterChanges :: StoreMap a -> [(Id a, ChangeOp, a)]
filterChanges = mapMaybe f . Map.toList
  where f (i, (Change op, a)) = Just (i, op, a)
        f (_, (Cached, _))    = Nothing

commit :: (Model a, MonadHexl m) => StoreMap a -> m ()
commit m = do
  let filterUpserts (i, op, a) = case op of
        Create -> Nothing
        Update -> Just $ Entity i a
        Delete -> Nothing
      filterDeletes (i, op, _) = case op of
        Create -> Nothing
        Update -> Nothing
        Delete -> Just i
      changes = filterChanges m
  upsertMany $ mapMaybe filterUpserts changes
  mapM_ delete $ mapMaybe filterDeletes changes

-- | Core of the engine logic
executeCommand :: MonadEngine m => Command -> m ()
executeCommand = \case

  CmdTableCreate projectId name ->
    void $ createTable $ Table projectId name

  CmdTableSetName tableId name -> do
    modifyTable tableId $ tableName .~ name
    -- Recompile every column that mentions this table in a formula
    dependantCols <- graphGets $ getTableDependantsOnly tableId $ \case
      TblDepColumnRef -> True
      TblDepTableRef  -> True
      TblDepRowRef    -> False
    mapM_ scheduleCompileColumn dependantCols

  CmdTableDelete tableId -> do
    deleteTable tableId
    columns <- getTableColumns tableId
    mapM_ (executeCommand . CmdColumnDelete . entityId) columns
    graphModify $ purgeTable tableId
    dependantCols <- graphGets $ getTableDependantsOnly tableId $ \case
      TblDepColumnRef -> False
      TblDepTableRef  -> False
      TblDepRowRef    -> True
    for_ dependantCols $ \columnId -> do
      -- TODO: invalidate rowrefs, but currently we have no concept of
      -- an "invalid type", so what to do?
      pure ()

  CmdDataColCreate tableId ->
    void $ createColumn (emptyDataCol tableId)

  CmdDataColUpdate columnId dataType isDerived code -> do
    oldCol <- getColumn columnId
    withDataCol oldCol $ \dataCol -> do
      -- Apply changes
      modifyColumn columnId $ columnKind . _ColumnData
        %~ (dataColType       .~ dataType)
         . (dataColIsDerived  .~ isDerived)
         . (dataColSourceCode .~ code)
      -- When the code has changed:
      when (dataCol ^. dataColSourceCode /= code) $
        scheduleCompileColumn columnId
      -- When the dataType has changed:
      when (dataCol ^. dataColType /= dataType) $ do
        scheduleCompileColumn columnId
        scheduleCompileColumnDependants (columnId, oldCol ^. columnTableId)
        when (isDerived == NotDerived) $ do
          cells <- getColumnCells columnId
          for_ cells $ \(Entity _ (Cell _ t c r)) -> do
            def <- makeDefaultValue dataType
            setAndPropagateCellContent t c r (CellValue def)
          let refDeps = getTypeDependencies dataType
          cycles <- graphSetTypeDependencies columnId refDeps
          -- TODO: Instead of throwing an error here, should mark the column as
          -- having a broken type.
          when cycles $ throwError $ ErrUser "Dependency graph contains cycles."
      -- When isDerived has changed:
      case (dataCol ^. dataColIsDerived, isDerived) of
        (NotDerived, Derived) ->
          scheduleCompileColumn columnId
        (Derived, NotDerived) -> do
          cells <- getColumnCells columnId
          for_ cells $ \(Entity _ (Cell content t c r)) -> case content of
            CellError _ -> do
              def <- makeDefaultValue dataType
              setAndPropagateCellContent t c r (CellValue def)
            CellValue _ -> pure ()
        _ -> pure ()

  CmdReportColCreate tableId ->
    void $ createColumn (emptyReportCol tableId)

  CmdReportColUpdate columnId template format language -> do
    oldCol <- getColumn columnId
    withReportCol oldCol $ \_ -> do
      modifyColumn columnId $ columnKind . _ColumnReport
        %~ (reportColTemplate .~ template)
         . (reportColFormat   .~ format)
         . (reportColLanguage .~ language)
    scheduleCompileColumn columnId

  CmdColumnSetName columnId name -> do
    modifyColumn columnId $ columnName .~ name
    column <- getColumn columnId
    scheduleCompileColumnDependants (columnId, column ^. columnTableId)

  CmdColumnDelete columnId -> do
    column <- getColumn columnId
    deleteColumn columnId
    scheduleCompileColumnDependants (columnId, column ^. columnTableId)
    graphModify $ purgeColumn columnId

  CmdRowCreate tableId -> do
    rowId <- createRow $ Row tableId
    columns <- getTableColumns tableId
    for_ columns $ \(Entity columnId column) ->
      case column ^? columnKind . _ColumnData of
        Nothing -> pure ()
        Just dataCol -> case dataCol ^. dataColIsDerived of
          NotDerived -> propagateCell tableId columnId rowId
          Derived    -> scheduleEvalCell columnId rowId

  CmdRowDelete rowId -> do
    row <- getRow rowId
    let tableId = row ^. rowTableId
    deleteRow rowId
    -- Recompile those dependants of any of the table's columns with an
    -- `AddAll` mode
    columns <- getTableColumns tableId
    for_ columns $ \(Entity columnId column) -> do
      dependants <- graphGets $
        getAllColumnDependants (columnId, column ^. columnTableId)
      for_ dependants $ \(depId, typ) -> case typ of
        AddAll -> scheduleCompileColumn depId
        AddOne -> pure ()
    -- For every cell that is part of a column that references this table in its
    -- type, we need to invalidate the reference in the value.
    refingCols <- graphGets $ getTableDependantsOnly tableId $ \case
      TblDepRowRef    -> True
      TblDepTableRef  -> False
      TblDepColumnRef -> False
    for_ refingCols $ \columnId -> do
      cells <- getColumnCells columnId
      for_ cells $ \(Entity _ cell) -> case cell ^. cellContent of
        CellError _ -> pure ()
        CellValue val -> case invalidateRowRef rowId val of
          Nothing -> pure ()
          Just newVal -> setAndPropagateCellContent
                           tableId columnId (cell ^. cellRowId)
                           (CellValue newVal)

  CmdCellSet columnId rowId value -> do
    column <- getColumn columnId
    setAndPropagateCellContent
      (column ^. columnTableId) columnId rowId
      (CellValue value)

--------------------------------------------------------------------------------

scheduleCompileColumnDependants :: MonadEngine m
                                => (Id Column, Id Table) -> m ()
scheduleCompileColumnDependants (columnId, tableId) = do
  dependants <- graphGets $ getAllColumnDependants (columnId, tableId)
  mapM_ (scheduleCompileColumn . fst) dependants

setAndPropagateCellContent :: MonadEngine m
                           => Id Table -> Id Column -> Id Row
                           -> CellContent -> m ()
setAndPropagateCellContent tableId columnId rowId content = do
  setCellContent columnId rowId content
  propagateCell tableId columnId rowId

propagateCell :: MonadEngine m => Id Table -> Id Column -> Id Row -> m ()
propagateCell tableId columnId rowId = do
  childs <- graphGets $ getAllColumnDependants (columnId, tableId)
  for_ childs $ \(childId, mode) ->
    case mode of
      AddOne -> scheduleEvalCell childId rowId
      AddAll -> scheduleEvalColumn childId
