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
  :<|> handleColumnSetName
  :<|> handleColumnSetType
  :<|> handleColumnSetInputType
  :<|> handleColumnSetSourceCode
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => Id Table -> m (Id Column)
handleColumnCreate tblId = create $
  Column tblId "" DataString ColumnInput "" CompiledCodeNone

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName c name = do
  update c $ \col -> col { columnName = name }
  -- TODO: re-compile dependent columns

handleColumnSetType :: MonadHexl m => Id Column -> DataType -> m ()
handleColumnSetType c typ = do
  update c $ \col -> col { columnType = typ }
  -- TODO: re-parse all cells
  -- TODO: re-compile this and all dependent columns

handleColumnSetInputType :: MonadHexl m => Id Column -> ColumnType -> m ()
handleColumnSetInputType c typ = do
  update c $ \col -> col { columnInputType = typ }
  -- TODO: re-parse all cells
  -- TODO: re-compile this and all dependent columns
  -- TODO: update dependency graph

-- | Just = error
-- sets columnInputType to Derived
handleColumnSetSourceCode :: MonadHexl m => Id Column -> Text -> m CompiledCode
handleColumnSetSourceCode c code = do
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
      propagate c CompleteColumn
      pure $ CompiledCode atexpr
  update c $ \col -> col { columnSourceCode = code
                         , columnCompiledCode = result
                         , columnInputType = ColumnDerived
                         }
  pure result

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
  result <- parseCell c r inp
  -- TODO: fork this
  propagate c (OneRecord r)
  pure result

-- Helper ----------------------------

compileColumn :: MonadHexl m => Id Column -> Text -> m (Either Text ATExpr)
compileColumn c code = case parseExpression code of
  Left msg -> pure $ Left $ "parse error: " <> msg
  Right expr -> do
    col <- getById' c
    typecheck (columnTableId col) expr (columnType col)

parseCell :: MonadHexl m => Id Column -> Id Record -> Text -> m CellResult
parseCell c r inp = do
  col <- getById' c
  let parsedValue = case columnType col of
        DataString -> ValueString <$> parseValue inp
        DataNumber -> ValueNumber <$> parseValue inp
        _          -> Nothing
      result = case parsedValue of
        Just val -> CellOk val
        Nothing -> CellParseError "could not parse"
      query =
        [ "aspects.columnId" =: toObjectId c
        , "aspects.recordId" =: toObjectId r
        ]
  updateByQuery' query $ \cell -> cell { cellInput = Just inp
                                       , cellResult = result
                                       }
  pure result
