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

import           Engine.Compile
import           Engine.Monad
import           Engine.Propagate
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
  | CmdColumnDelete (Id Column)
  | CmdColumnSetName (Id Column) Text
  | CmdRowCreate (Id Table)
  | CmdRowDelete (Id Row)
  | CmdCellSet (Id Column) (Id Row) Value

--------------------------------------------------------------------------------

runCommand :: MonadHexl m => Command -> m ()
runCommand cmd = do
  graph <- undefined -- get initial
  endState <- runEngine graph (executeCommand cmd)
  undefined endState -- commit changes

executeCommand :: MonadEngine h m => Command -> m ()
executeCommand = \case

  CmdTableCreate projectId name ->
    void $ tableCreate $ Table projectId name

  CmdTableSetName tableId name -> do
    tableModify tableId $ tableName .~ name
    -- Recompile every column that mentions this table in a formula
    dependantCols <- graphGets $ getTableDependants tableId
    let relevantCols = map fst $ flip filter dependantCols $ \(_, typ) ->
          case typ of
            TblDepColumnRef -> True
            TblDepTableRef  -> True
            TblDepRow       -> False
    mapM_ compileColumn relevantCols
    -- Propagate
    propagate [ RootWholeColumns relevantCols ]

  CmdTableDelete tableId -> tableDelete tableId

  CmdDataColCreate tableId -> do
    columnId <- columnCreate (emptyDataCol tableId)
    -- Create a new cell for every row in the table
    rows <- liftDB $ listByQuery [ "tableId" =: toObjectId tableId ]
    for_ rows $ \(Entity rowId _) -> do
      defContent <- liftDB $ defaultContent emptyDataColType
      cellCreate $ newCell tableId columnId rowId defContent

  CmdDataColUpdate columnId dataType isDerived code -> do
    oldCol <- getColumn columnId
    withDataCol oldCol $ \dataCol -> do
      -- Apply changes
      columnModify columnId $ columnKind . _ColumnData
        %~ (dataColType .~ dataType)
         . (dataColIsDerived .~ isDerived)
         . (dataColSourceCode .~ code)
      -- If the dataType has changed, we need to re-compile all the columns
      -- that depend on this one.
      when (dataCol ^. dataColType /= dataType) $
        registerCompileColumnDependants (columnId, oldCol ^. columnTableId)
      case isDerived of
        Derived -> registerCompileTarget columnId
        NotDerived -> if dataCol ^. dataColType == dataType
          then do
            cells <- getColumnCells columnId
            for_ cells $ \(Cell _ c r content) -> case content of
              CellError _ -> -- TODO:
              CellValue _ -> pure ()
            -- only new defaults for error cells:
            -- for every cell: if error, set defaultContent
                        
          else -- updateReference dependencies
               -- new defaults for new datatype

  CmdReportColCreate tableId ->
    void $ columnCreate (emptyReportCol tableId)

  CmdReportColUpdate columnId template format language -> do
    undefined

--------------------------------------------------------------------------------

registerCompileColumnDependants :: MonadEngine m => (Id Column, Id Table) -> m ()
registerCompileColumnDependants (columnId, tableId) = do
  dependants <- graphGets $ getAllColumnDependants (columnId, tableId)
  mapM_ registerCompileTarget dependants

setAndPropagateCellContent :: Id Table -> Id Column -> Id Row -> CellContent -> m ()
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

withDataCol :: Monad m => Column -> (DataCol -> m a) -> m a
withDataCol col f = case col ^? columnKind . _ColumnData of
  Nothing      -> throwError $ ErrBug "withDataCol failed"
  Just dataCol -> f dataCol
