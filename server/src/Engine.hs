{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
-- |

module Engine
  ( Command (..)
  , runCommand
  ) where

import           Control.Lens
import           Control.Monad.Except

import           Data.Foldable                (for_)
import           Data.Text                    (Text)

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Engine.Monad
import           Engine.Util
import           Monads

--------------------------------------------------------------------------------

-- | All the critical commands that should be atomic, undoable, replayable etc.
-- within a project.
data Command
  = CmdTableCreate (Id Project) Text
  -- ^ TODO: In the future, this should include ids to be deterministic
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

--------------------------------------------------------------------------------

runCommand :: MonadHexl m => Command -> m ()
runCommand cmd = do
  graph <- undefined -- get initial
  endState <- runEngineT graph (executeCommand cmd)
  undefined endState -- commit changes

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

  CmdDataColCreate tableId -> do
    columnId <- createColumn (emptyDataCol tableId)
    -- Create a new cell for every row in the table
    rows <- getTableRows tableId
    for_ rows $ \(Entity rowId _) -> do
      def <- makeDefaultValue emptyDataColType
      createCell $ newCell tableId columnId rowId (CellValue def)

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
        Just dataCol -> do
          def <- makeDefaultValue (dataCol ^. dataColType)
          void $ createCell $ newCell tableId columnId rowId (CellValue def)
          case dataCol ^. dataColIsDerived of
            NotDerived -> setAndPropagateCellContent tableId columnId rowId
                                                     (CellValue def)
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
  childs <- graphGets $ getAllColumnDependants (columnId, tableId)
  for_ childs $ \(childId, mode) ->
    case mode of
      AddOne -> scheduleEvalCell childId rowId
      AddAll -> scheduleEvalColumn childId
