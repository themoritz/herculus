{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase        #-}

module Handler.Rest where

import           Control.Monad (unless, void)

import           Data.Text        (Text)
import           Data.Monoid
import           Data.Foldable
import           Data.Traversable

import           Servant

import           Database.MongoDB ((=:))

import           Lib.Types
import           Lib.Api.WebSocket
import           Lib.Model.Types
import           Lib.Model.Column
import           Lib.Model.Cell
import           Lib.Model
import           Lib.Api.Rest
import           Monads
import           Lib.Model.Dependencies
import           Propagate
import           Propagate.Monad
import           Lib.Expression
import           Typecheck

handle :: MonadHexl m => ServerT Routes m
handle =
       handleProject
  :<|> handleTable
  :<|> handleColumn
  :<|> handleRecord
  :<|> handleCell

--

handleProject :: MonadHexl m => ServerT ProjectRoutes m
handleProject =
       handleProjectCreate
  :<|> handleProjectList

handleProjectCreate :: MonadHexl m => Project -> m (Id Project)
handleProjectCreate = create

handleProjectList :: MonadHexl m => m [Entity Project]
handleProjectList = listAll

--

handleTable :: MonadHexl m => ServerT TableRoutes m
handleTable =
       handleTableCreate
  :<|> handleTableList
  :<|> handleTableData

handleTableCreate :: MonadHexl m => Table -> m (Id Table)
handleTableCreate = create

handleTableList :: MonadHexl m => Id Project -> m [Entity Table]
handleTableList projId = listByQuery [ "projectId" =: toObjectId projId ]

handleTableData :: MonadHexl m => Id Table -> m [(Id Column, Id Record, CellContent)]
handleTableData tblId = do
  cells <- listByQuery [ "aspects.tableId" =: toObjectId tblId ]
  let go (Cell v (Aspects _ c r)) = (c, r, v)
  pure $ map (go . entityVal) cells

--

handleColumn :: MonadHexl m => ServerT ColumnRoutes m
handleColumn =
       handleColumnCreate
  :<|> handleColumnSetName
  :<|> handleColumnSetDataType
  :<|> handleColumnSetInput
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => Id Table -> m (Id Column)
handleColumnCreate t = do
  c <- create $ Column t "" DataString ColumnInput "" CompileResultNone
  rs <- listByQuery [ "tableId" =: toObjectId t ]
  for_ rs $ \e -> create $ emptyCell t c (entityId e)
  pure c

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName c name = do
  update c $ \col -> col { columnName = name }
  compileColumnChildren c

handleColumnSetDataType :: MonadHexl m => Id Column -> DataType -> m ()
handleColumnSetDataType c typ = do
  oldCol <- getById' c
  update c $ \col -> col { columnDataType = typ }
  unless (columnDataType oldCol == typ) $ do
    compileColumnChildren c
    case columnInputType oldCol of
      ColumnInput -> invalidateCells c
      ColumnDerived -> compileColumn c

-- | Just = error
-- sets columnInputType to Derived
handleColumnSetInput :: MonadHexl m => Id Column -> (InputType, Text) -> m ()
handleColumnSetInput c (typ, code) = do
  oldCol <- getById' c
  update c $ \col -> col { columnInputType = typ
                         , columnSourceCode = code }
  case (columnInputType oldCol, columnSourceCode oldCol == code) of
    (ColumnDerived, False) -> compileColumn c
    _                      -> pure ()

handleColumnList :: MonadHexl m => Id Table -> m [Entity Column]
handleColumnList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleRecord :: MonadHexl m => ServerT RecordRoutes m
handleRecord =
       handleRecordCreate
  :<|> handleRecordList

handleRecordCreate :: MonadHexl m => Id Table -> m (Id Record)
handleRecordCreate t = do
  r <- create $ Record t
  cs <- listByQuery [ "tableId" =: toObjectId t ]
  for_ cs $ \e -> create $ emptyCell t (entityId e) r
  -- TODO: propagate all columns with the new record
  pure r

handleRecordList :: MonadHexl m => Id Table -> m [Entity Record]
handleRecordList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleCell :: MonadHexl m => ServerT CellRoutes m
handleCell =
       handleCellSet

handleCellSet :: MonadHexl m => Id Column -> Id Record -> Value -> m ()
handleCellSet c r val = do
  let query =
        [ "aspects.columnId" =: toObjectId c
        , "aspects.recordId" =: toObjectId r
        ]
  updateByQuery' query $ \cell -> cell { cellContent = CellValue val }
  -- TODO: fork this
  propagate c (OneRecord r)

-- Helper ----------------------------

compileColumn :: MonadHexl m => Id Column -> m ()
compileColumn c = do
  col <- getById' c
  let abort msg = do
        void $ modifyDependencies c []
        pure $ CompileResultError msg
  compileResult <- case parseExpression (columnSourceCode col) of
    Left msg -> abort $ "Parse error: " <> msg
    Right expr -> do
      atexprRes <- typecheck (columnTableId col) expr (columnDataType col)
      case atexprRes of
        Left msg -> abort $ "Type error: " <> msg
        Right atexpr -> do
          let deps = collectDependencies atexpr
          cycles <- modifyDependencies c deps
          if cycles then abort "Dependency graph has cycles"
                    else pure $ CompileResultCode atexpr
  update c $ \col' -> col' { columnCompileResult = compileResult }
  sendWS $ WsDownColumnsChanged [Entity c col { columnCompileResult = compileResult }]
  propagate c CompleteColumn

compileColumnChildren :: MonadHexl m => Id Column -> m ()
compileColumnChildren c = do
  graph <- getDependencies
  entries <- for (getChildren c graph) $ \(child, _) -> do
    compileColumn child
    childCol <- getById' child
    pure $ Entity child childCol
  sendWS $ WsDownColumnsChanged entries

invalidateCells :: MonadHexl m => Id Column -> m ()
invalidateCells c = do
  cells <- listByQuery [ "aspects.columnId" =: toObjectId c]
  changes <- for cells $ \e -> do
    update (entityId e) $ \cell -> cell { cellContent = CellNothing }
    let aspects = cellAspects $ entityVal e
    pure (aspectsColumnId aspects, aspectsRecordId aspects, CellNothing)
  sendWS $ WsDownCellsChanged changes
  propagate c CompleteColumn
