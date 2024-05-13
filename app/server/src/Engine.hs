{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
-- |

module Engine
  ( Command (..)
  , runCommands
  , commitEngineState
  ) where

import           Lib.Prelude

import           Control.Lens                 hiding (Getter, op, (&))

import qualified Data.Map                     as Map
import qualified Data.Text                    as T (length)

import           Lib.Api.Schema.Column        (columnFromEntity)
import           Lib.Api.Schema.Compiler
import           Lib.Api.Schema.Project       (Command (..), projectFromEntity)
import           Lib.Api.WebSocket
import           Lib.Compiler
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Common
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           ConnectionManager            (getConnectionsToProject)
import           Engine.Compile
import           Engine.Monad
import           Engine.Propagate
import           Engine.Util
import           Monads

runCommands :: MonadHexl m => Id Project -> [Command] -> m ()
runCommands projectId cmds = do
  p <- getById' projectId
  -- Run command in engine, compile, and propagate
  (_ , st) <- runEngineT (Entity projectId p) $ do
    mapM_ executeCommand cmds
    getCompileTargets >>= traverse_ compileColumn
    propagate
  -- Send changes to clients
  broadcastStoreChanges projectId st
  -- Commit changes
  commitEngineState projectId st

broadcastStoreChanges :: MonadHexl m => Id Project -> EngineState -> m ()
broadcastStoreChanges projectId st = do
  let Store {..} = st ^. engineStore
  connections <- withConnectionMgr $ getConnectionsToProject projectId
  oldProject <- getById' projectId
  let
    old = projectFromEntity (Entity projectId oldProject)
    new = projectFromEntity (Entity projectId (st ^. engineProject))
  sendWS connections $ WsDownProjectDiff
    projectId
    (if old /= new then Just new else Nothing)
    (filterChanges _storeCells)
    (filterChanges _storeColumns & traverse . _2 %~ columnFromEntity)
    (filterChanges _storeRows)
    (filterChanges _storeTables)

filterChanges :: StoreMap a -> [(ChangeOp, Entity a)]
filterChanges = mapMaybe f . Map.toList
  where f (i, (Change op, a)) = Just (op, Entity i a)
        f (_, (Cached, _))    = Nothing

commitEngineState :: MonadDB m => Id Project -> EngineState -> m ()
commitEngineState projectId st = do
  let Store{..} = st ^. engineStore
  update projectId (const (st ^. engineProject))
  commit _storeCells
  commit _storeColumns
  commit _storeRows
  commit _storeTables

commit :: (Model a, MonadDB m) => StoreMap a -> m ()
commit m = do
  let filterUpserts (op, Entity i a) = case op of
        Create -> Nothing
        Update -> Just $ Entity i a
        Delete -> Nothing
      filterDeletes (op, Entity i _) = case op of
        Create -> Nothing
        Update -> Nothing
        Delete -> Just i
      changes = filterChanges m
  upsertMany $ mapMaybe filterUpserts changes
  mapM_ delete $ mapMaybe filterDeletes changes

-- | Core of the engine logic
executeCommand :: MonadEngine m => Command -> m ()
executeCommand = \case

  CmdProjectSetName name ->
    modifyProject (projectName .~ name)

  CmdProjectSetModule src -> do
    modifyProject (projectModuleSource .~ src)
    compileModule src voidResolver preludeCheckEnv >>= \case
      Left _ -> pure ()
      Right checkResult -> do
        let result = CompileResultOk $ checkResultToModule checkResult
        modifyProject (projectModule .~ result)
        tables <- getProjectTables
        for_ tables $ \(Entity tableId _) -> do
          cols <- getTableColumns tableId
          for_ cols $ \(Entity columnId col) -> case col ^. columnKind of
            ColumnData dataCol -> case dataCol ^. dataColIsDerived of
              Derived    -> scheduleCompileColumn columnId
              NotDerived -> pure ()
            ColumnReport _ -> scheduleCompileColumn columnId

  CmdTableCreate name -> do
    i <- askProjectId
    getTableByName (Ref name) >>= \case
      Just _ -> throwError $ ErrUser "A table with that name already exists."
      Nothing -> void $ createTable $ Table i name

  CmdTableSetName tableId name -> do
    getTableByName (Ref name) >>= \case
      Just _ -> throwError $ ErrUser "A table with that name already exists."
      Nothing -> pure ()
    modifyTable tableId $ tableName .~ name
    -- Recompile every column that mentions this table in a formula
    dependantCols <- graphGets $ getTableDependantsOnly tableId $ \case
      TblDepColumnRef -> True
      TblDepTableRef  -> True
      TblDepRowRef    -> False
    mapM_ scheduleCompileColumn dependantCols

  CmdTableDelete tableId -> do
    columns <- getTableColumns tableId
    mapM_ (executeCommand . CmdColumnDelete . entityId) columns
    mentioningCols <- graphGets $ getTableDependantsOnly tableId $ \case
      TblDepColumnRef -> True
      TblDepTableRef  -> True
      TblDepRowRef    -> False
    mapM_ scheduleCompileColumn mentioningCols
    rowRef'ingCols <- graphGets $ getTableDependantsOnly tableId $ \case
      TblDepColumnRef -> False
      TblDepTableRef  -> False
      TblDepRowRef    -> True
    for_ rowRef'ingCols $ \_columnId -> do
      -- TODO: invalidate rowrefs, but currently we have no concept of
      -- an "invalid type", so what to do?
      pure ()
    graphModify $ purgeTable tableId
    deleteTable tableId

  CmdDataColCreate tableId ->
    void $ createColumn (emptyDataCol tableId)

  CmdDataColUpdate columnId dataType isDerived code -> do
    when (T.length code > 1000) $ throwError $
      ErrUser "The length of formulas is currently limited to 1000 characters."
    oldCol <- getColumn columnId
    withDataCol oldCol $ \dataCol -> do
      -- Apply changes
      modifyColumn columnId $ columnKind . _ColumnData
        %~ (dataColType       .~ dataType)
         . (dataColIsDerived  .~ isDerived)
         . (dataColSourceCode .~ code)
      -- Always recompile if the column is derived:
      when (isDerived == Derived) $
        scheduleCompileColumn columnId
      -- When the dataType has changed:
      when (dataCol ^. dataColType /= dataType) $ do
        scheduleCompileColumn columnId
        scheduleCompileColumnDependants columnId
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
    when (T.length template > 10000) $ throwError $
      ErrUser "The length of templates is currently limited to 10000 characters."
    oldCol <- getColumn columnId
    withReportCol oldCol $ \_ ->
      modifyColumn columnId $ columnKind . _ColumnReport
        %~ (reportColTemplate .~ template)
         . (reportColFormat   .~ format)
         . (reportColLanguage .~ language)
    scheduleCompileColumn columnId

  CmdColumnSetName columnId name -> do
    column <- getColumn columnId
    getColumnOfTableByName (column ^. columnTableId) (Ref name) >>= \case
      Just _ -> throwError $ ErrUser "A column with that name already exists."
      Nothing -> pure ()
    modifyColumn columnId $ columnName .~ name
    scheduleCompileColumnDependants columnId

  CmdColumnDelete columnId -> do
    scheduleCompileColumnDependants columnId
    graphModify $ purgeColumn columnId
    deleteColumn columnId

  CmdRowCreate tableId -> do
    rows <- getTableRows tableId
    when (length rows >= 200) $ throwError $
      ErrUser "Tables are currently limited to have at most 200 rows."
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
    -- Evaluate all cells of those dependants of any of the table's columns
    -- with an `AddAll` mode.
    columns <- getTableColumns tableId
    for_ columns $ \(Entity columnId column) -> do
      dependants <- graphGets $
        getAllColumnDependants (columnId, column ^. columnTableId)
      for_ dependants $ \(depId, typ) -> case typ of
        AddAll -> scheduleEvalColumn depId
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
    deleteRow rowId

  CmdCellSet columnId rowId value -> do
    column <- getColumn columnId
    case column ^. columnKind of
      ColumnReport _ -> throwError $
        ErrUser "Cannot set content of report cell."
      ColumnData dataCol -> case dataCol ^. dataColIsDerived of
        Derived -> throwError $
          ErrUser "Cannot set cell content of a derived cell."
        NotDerived ->
          setAndPropagateCellContent
            (column ^. columnTableId) columnId rowId
            (CellValue value)

--------------------------------------------------------------------------------

scheduleCompileColumnDependants :: MonadEngine m => Id Column -> m ()
scheduleCompileColumnDependants columnId = do
  dependants <- graphGetsM $
    getColumnCompileDependants columnId getColumnTableId
  mapM_ scheduleCompileColumn dependants

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
