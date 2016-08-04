{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase        #-}

module Handler.Rest where


import           Data.Text        (Text)
import           Data.Monoid
import           Data.Foldable

import           Servant

import           Database.MongoDB ((=:))

import           Lib.Types
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
  :<|> handleColumnSetInputType
  :<|> handleColumnSetSourceCode
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => Id Table -> m (Id Column)
handleColumnCreate t = do
  c <- create $ Column t "" DataString ColumnInput "" CompiledCodeNone
  rs <- listByQuery [ "tableId" =: toObjectId t ]
  for_ rs $ \e -> create $ emptyCell t c (entityId e)
  pure c

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName c name = do
  update c $ \col -> col { columnName = name }
  -- TODO: re-compile dependent columns

handleColumnSetDataType :: MonadHexl m => Id Column -> DataType -> m ()
handleColumnSetDataType c typ = do
  update c $ \col -> col { columnDataType = typ }
  -- TODO: re-parse all cells
  -- TODO: re-compile this and all dependent columns

handleColumnSetInputType :: MonadHexl m => Id Column -> InputType -> m ()
handleColumnSetInputType c typ = do
  update c $ \col -> col { columnInputType = typ }
  -- TODO: re-parse all cells
  -- TODO: re-compile this and all dependent columns
  -- TODO: update dependency graph

-- | Just = error
-- sets columnInputType to Derived
handleColumnSetSourceCode :: MonadHexl m => Id Column -> Text -> m (Maybe CompiledCode)
handleColumnSetSourceCode c code = do
  update c $ \col -> col { columnSourceCode = code }
  col <- getById' c
  case columnInputType col of
    ColumnInput -> pure Nothing
    ColumnDerived -> do
      compileResult <- compileColumn c code
      result <- case compileResult of
        Left msg -> do
          modifyDependencies $ setDependencies c []
          pure $ CompiledCodeError msg
        Right atexpr -> do
          let deps = collectDependencies atexpr
          -- TODO: check for cycles
          modifyDependencies $ setDependencies c deps
          -- TODO: fork this
          update c $ \col -> col { columnCompiledCode = CompiledCode atexpr }
          propagate c CompleteColumn
          pure $ CompiledCode atexpr
      pure $ Just result

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

compileColumn :: MonadHexl m => Id Column -> Text -> m (Either Text ATExpr)
compileColumn c code = case parseExpression code of
  Left msg -> pure $ Left $ "parse error: " <> msg
  Right expr -> do
    col <- getById' c
    typecheck (columnTableId col) expr (columnDataType col)
