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
import           Database.MongoDB             ((=:))

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

executeCommand :: MonadEngine h m => Command -> m ()
executeCommand = \case

  CmdTableCreate projectId name ->
    void $ createTable $ Table projectId name

  CmdTableSetName tableId name -> do
    modifyTable tableId $ tableName .~ name
    -- Recompile every column that mentions this table in a formula
    dependantCols <- graphGets $ getTableDependants tableId
    let relevantCols = map fst $ flip filter dependantCols $ \(_, typ) ->
          case typ of
            TblDepColumnRef -> True
            TblDepTableRef  -> True
            TblDepRowRef    -> False
    mapM_ scheduleCompileColumn relevantCols

  CmdTableDelete tableId -> do
    deleteTable tableId
    -- TODO: Purge dependencies
    -- TODO: Recompile dependent columns as if containing columns have changed

  CmdDataColCreate tableId -> do
    columnId <- createColumn (emptyDataCol tableId)
    -- Create a new cell for every row in the table
    rows <- liftDB $ listByQuery [ "tableId" =: toObjectId tableId ]
    for_ rows $ \(Entity rowId _) -> do
      defContent <- liftDB $ defaultContent emptyDataColType
      createCell $ newCell tableId columnId rowId defContent

  -- TODO: structure in three parts for when type, derived and code changed
  -- respectively
  CmdDataColUpdate columnId dataType isDerived code -> do
    oldCol <- getColumn columnId
    withDataCol oldCol $ \dataCol -> do
      -- Apply changes
      modifyColumn columnId $ columnKind . _ColumnData
        %~ (dataColType       .~ dataType)
         . (dataColIsDerived  .~ isDerived)
         . (dataColSourceCode .~ code)
      -- If the dataType has changed, we need to re-compile all the columns
      -- that depend on this one.
      when (dataCol ^. dataColType /= dataType) $
        scheduleCompileColumnDependants (columnId, oldCol ^. columnTableId)
      case isDerived of
        Derived ->
          scheduleCompileColumn columnId
        NotDerived -> do
          cells <- getColumnCells columnId
          if dataCol ^. dataColType == dataType
            then for_ cells $ \(Entity _ (Cell content t c r)) -> case content of
              CellError _ -> do
                def <- liftDB $ defaultContent dataType
                setAndPropagateCellContent t c r def
              CellValue _ -> pure ()
            else for_ cells $ \(Entity _ (Cell _ t c r)) -> do
              def <- liftDB $ defaultContent dataType
              setAndPropagateCellContent t c r def
              -- TODO: update reference dependencies

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
    -- TODO: remove dependencies

  CmdRowCreate tableId -> do
    rowId <- createRow $ Row tableId
    columns <- getTableColumns tableId
    for_ columns $ \(Entity columnId column) ->
      case column ^? columnKind . _ColumnData of
        Nothing -> pure ()
        Just dataCol -> do
          def <- liftDB $ defaultContent (dataCol ^. dataColType)
          void $ createCell $ newCell tableId columnId rowId def
          case dataCol ^. dataColIsDerived of
            NotDerived -> setAndPropagateCellContent tableId columnId rowId def
            Derived    -> scheduleEvalCell columnId rowId

  CmdRowDelete rowId -> do
    row <- getRow rowId
    let tableId = row ^. rowTableId
    deleteRow rowId
    columns <- getTableColumns tableId
    -- TODO: Only those with `AddAll` mode
    mapM_ (scheduleCompileColumnDependants . (,tableId) . entityId) columns
    -- For every cell that is part of a column that references this table in its
    -- type, we need to invalidate the reference in the value.
    refingCols <- graphGets $ getTableDependantsOnly TblDepRowRef tableId
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

scheduleCompileColumnDependants :: MonadEngine db m => (Id Column, Id Table) -> m ()
scheduleCompileColumnDependants (columnId, tableId) = do
  dependants <- graphGets $ getAllColumnDependants (columnId, tableId)
  mapM_ (scheduleCompileColumn . fst) dependants

setAndPropagateCellContent :: MonadEngine db m => Id Table -> Id Column -> Id Row -> CellContent -> m ()
setAndPropagateCellContent tableId columnId rowId content = do
  setCellContent columnId rowId content
  childs <- graphGets $ getAllColumnDependants (columnId, tableId)
  for_ childs $ \(childId, mode) ->
    case mode of
      AddOne -> scheduleEvalCell childId rowId
      AddAll -> scheduleEvalColumn childId

-- TODO: configurable by user
defaultContent :: MonadHexl m => DataType -> m CellContent
defaultContent = \case
  DataBool     -> pure . CellValue $ VBool False
  DataString   -> pure . CellValue $ VString ""
  DataNumber   -> pure . CellValue $ VNumber 0
  DataTime     -> CellValue . VTime <$> getCurrentTime
  DataRowRef t -> do
    res <- getOneByQuery [ "tableId" =: toObjectId t ]
    pure $ CellValue $ VRowRef $ case res of
      Left _             -> Nothing
      Right (Entity i _) -> Just i
  DataList _   -> pure . CellValue $ VList []
  DataMaybe _  -> pure . CellValue $ VMaybe Nothing
