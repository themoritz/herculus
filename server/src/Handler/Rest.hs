{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Rest where


import           Data.Text        (Text)

import           Servant

import           Database.MongoDB ((=:))

import           Lib.Types
import           Lib.Model.Types
import           Lib.Model
import           Lib.Api.Rest
import           Monads
import           Lib.Model.Dependencies
import           Propagate
import           Lib.Expression
import           Eval

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

handleProjectList :: MonadHexl m => m [Project]
handleProjectList = map entityVal <$> listAll

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

handleTableData :: MonadHexl m => Id Table -> m [(Id Column, Id Record, Value)]
handleTableData tblId = do
  cells <- listByQuery [ "aspects.tableId" =: toObjectId tblId ]
  let go (Cell v (Aspects _ c r)) = (c, r, v)
  pure $ map (go . entityVal) cells

--

handleColumn :: MonadHexl m => ServerT ColumnRoutes m
handleColumn =
       handleColumnCreate
  :<|> handleColumnSetName
  :<|> handleColumnSetType
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => Id Table -> m (Id Column)
handleColumnCreate tblId = create $ Column tblId "" (ColumnInput DataString)

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName colId name =
  update colId (\col -> col { columnName = name })

handleColumnSetType :: MonadHexl m => Id Column -> ColumnType -> m ()
handleColumnSetType colId cType = do
  update colId (\col -> col { columnType = cType })
  -- Get old dependencies
  graph <- getDependencyGraph
  graph' <- case cType of
    ColumnInput _ -> pure $ setDependencies colId [] graph
    ColumnDerived formula -> case parseExpression formula of
      Left _ -> pure $ setDependencies colId [] graph
      Right expr -> do
        deps <- collectDependencies expr
        let graph' = setDependencies colId deps graph
            topoResult = getDependentTopological colId graph'
        case topoResult of
          Nothing -> pure $ setDependencies colId [] graph
          Just order -> do
            -- TODO: Propagate
            pure graph'
  storeDependencyGraph graph'

handleColumnList :: MonadHexl m => Id Table -> m [Entity Column]
handleColumnList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleRecord :: MonadHexl m => ServerT RecordRoutes m
handleRecord =
       handleRecordCreate
  :<|> handleRecordList

handleRecordCreate :: MonadHexl m => Id Table -> m (Id Record)
handleRecordCreate = create . Record

handleRecordList :: MonadHexl m => Id Table -> m [Entity Record]
handleRecordList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleCell :: MonadHexl m => ServerT CellRoutes m
handleCell =
       handleCellSet

handleCellSet :: MonadHexl m => Cell -> m ()
handleCellSet cell@(Cell _ (Aspects tblId colId recId)) = do
  let query =
        [ "aspects" =:
          [ "columnId" =: toObjectId colId -- "concept"
          , "recordId" =: toObjectId recId
          , "tableId"  =: toObjectId tblId
          ]
        ]
  upsert query cell
  graph <- getDependencyGraph
  let mOrder = getDependentTopological colId graph
  case mOrder of
    Just order -> runPropagate recId order
    Nothing -> throwError $ ErrBug "Dependency graph has cycles"
