{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase        #-}

module Handler.Rest where

import           Control.Monad.Logger

import           Data.Text        (Text, pack)
import           Data.Monoid

import           Text.Show.Pretty (ppShow)

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
import           Eval
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

handleTableData :: MonadHexl m => Id Table -> m [(Id Column, Id Record, CellResult)]
handleTableData tblId = do
  cells <- listByQuery [ "aspects.tableId" =: toObjectId tblId ]
  let go (Cell _ v (Aspects _ c r)) = (c, r, v)
  pure $ map (go . entityVal) cells

--

handleColumn :: MonadHexl m => ServerT ColumnRoutes m
handleColumn =
       handleColumnCreate
  :<|> handleColumnUpdate
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => Id Table -> m (Id Column)
handleColumnCreate tblId = create $ Column tblId "" DataString ColumnInput "" CompiledCodeNone

handleColumnUpdate :: MonadHexl m => Id Column -> Column -> m ()
handleColumnUpdate colId newCol = do
  update colId (const newCol)
  -- Get old dependencies
  graph <- getDependencyGraph
  case columnInputType newCol of
    ColumnInput -> storeDependencyGraph $ setDependencies colId [] graph
    ColumnDerived -> case parseExpression (columnSourceCode newCol) of
      Left msg -> do
        storeDependencyGraph $ setDependencies colId [] graph
        throwError $ ErrUser $ "cannot parse expression: " <> msg
      Right expr -> do
        (Column t _ _ _ _ _) <- getById' colId
        runTypecheck t expr (columnType newCol) >>= \case
          Left msg -> do
            storeDependencyGraph $ setDependencies colId [] graph
            throwError $ ErrUser $ "type error: " <> msg
          Right atexpr -> do
            let deps = collectDependencies atexpr
                graph' = setDependencies colId deps graph
                mOrder = getDependentTopological colId graph'
            case mOrder of
              Nothing -> throwError $ ErrUser "Dependency graph has cycles"
              Just order -> do
                records <- listByQuery [ "tableId" =: t ]
                storeDependencyGraph graph'
                propagate colId CompleteColumn

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

handleCellSet :: MonadHexl m => Id Column -> Id Record -> Text -> m CellResult
handleCellSet c r inp = do
  upsertCell cell
  propagate c (OneRecord r)
