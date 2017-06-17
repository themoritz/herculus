{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |

module Engine.Compile
  ( compileColumn
  , checkDataCol
  , checkReportCol
  ) where

import           Lib.Prelude

import           Control.Lens

import qualified Data.Map                  as Map
import           Data.Maybe                (mapMaybe)

import           Lib.Api.Schema.Compiler   (moduleToCheckResult)
import           Lib.Compiler
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Check.Monad  (ResolveF (..), Resolver)
import           Lib.Compiler.Check.Types
import           Lib.Compiler.Core
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Type
import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Common
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Template
import           Lib.Template.Core
import           Lib.Types

import           Engine.Monad
import           Monads

--------------------------------------------------------------------------------

-- | Compiles a column (both data and report columns).
--
-- For a data column: compile, check the inferred type matches the column's
-- type, extract the dependencies from the expression, check for cycles in
-- the dependency graph, and update the dependencies of the column.
--
-- For a report column: compile and update the dependencies.
compileColumn :: forall m. MonadEngine m => Id Column -> m ()
compileColumn columnId = do
  col <- getColumn columnId
  case col ^. columnKind of
    ColumnData dataCol -> do
      compileResult <- checkDataCol
        (col ^. columnTableId) columnId
        (dataCol ^. dataColType) (dataCol ^. dataColSourceCode)
      modifyColumn columnId $
        columnKind . _ColumnData . dataColCompileResult .~ compileResult
    ColumnReport repCol -> do
      compileResult <- checkReportCol
        (col ^. columnTableId) columnId (repCol ^. reportColTemplate)
      modifyColumn columnId $
        columnKind . _ColumnReport . reportColCompiledTemplate .~ compileResult

abort :: MonadEngine m => Id Column -> [Error] -> m (CompileResult a)
abort c errs = do
  void $ graphSetCodeDependencies c mempty
  pure $ CompileResultError errs

withCheckEnv
  :: MonadEngine m => Id Column
  -> (CheckEnv -> m (CompileResult a)) -> m (CompileResult a)
withCheckEnv c action =
  useProject projectModule >>= \case
    CompileResultOk modu -> do
      let checkEnv = unionCheckEnv
            preludeCheckEnv (resultCheckEnv $ moduleToCheckResult modu)
      action checkEnv
    CompileResultNone ->
      action preludeCheckEnv
    CompileResultError _ ->
      abort c [Error "Project module contains errors." voidSpan]

checkDataCol
  :: MonadEngine m
  => Id Table -> Id Column -> DataType -> Text -> m DataCompileResult
checkDataCol t c dt src = do
  let colType = typeOfDataType dt
  withCheckEnv c $ \checkEnv ->
    compileFormulaWithType src colType (mkResolver t) checkEnv >>= \case
      Left err -> abort c [err]
      Right expr -> do
        let deps = collectCodeDependencies expr
        cycles <- graphSetCodeDependencies c deps
        if cycles then abort c [Error "Dependency graph has cycles." voidSpan]
                  else pure $ CompileResultOk expr

checkReportCol
  :: MonadEngine m
  => Id Table -> Id Column -> Text -> m ReportCompileResult
checkReportCol t c src =
  withCheckEnv c $ \checkEnv ->
    compileTemplate src (mkResolver t) checkEnv >>= \case
      Left err -> abort c [err]
      Right tTpl -> do
        let deps = collectTplCodeDependencies tTpl
        cycles <- graphSetCodeDependencies c deps
        when cycles $
          throwError $ ErrBug "Setting report dependencies generated cycle"
        pure $ CompileResultOk tTpl

--------------------------------------------------------------------------------

mkResolver :: MonadEngine m => Id Table -> Resolver m
mkResolver ownTblId = \case
    ResolveColumnRef cRef reply -> map reply $
      (map.map) (\(_,i,c) -> (i,c)) $ resolveColumnRef ownTblId cRef

    ResolveColumnOfTableRef tblName colName reply -> do
      tableRes <- getTableByName tblName
      reply <$> case tableRes of
        Nothing           -> pure Nothing
        Just (Entity i _) -> resolveColumnRef i colName

    ResolveTableRef tblName reply -> do
      tableRes <- getTableByName tblName
      reply <$> case tableRes of
        Nothing               -> pure Nothing
        Just (Entity tblId _) -> pure $ Just tblId

    GetTableRecordType tblId reply -> reply <$> getTableRecordType tblId

    GetTableName tblId reply -> reply . view tableName <$> getTable tblId

resolveColumnRef
  :: MonadEngine m
  => Id Table -> Ref Column
  -> m (Maybe (Id Table, Id Column, DataCol))
resolveColumnRef tableId colName = do
  columnRes <- getColumnOfTableByName tableId colName
  pure $ case columnRes of
    Nothing             -> Nothing
    Just (Entity i col) -> fmap (tableId,i,)
                                (col ^? columnKind . _ColumnData)

getTableRecordType :: MonadEngine m => Id Table -> m (Map Text Type)
getTableRecordType t = do
  cols <- getTableColumns t
  let goCol (Entity _ col) = case col ^. columnKind of
        ColumnData dataCol ->
          Just (col ^. columnName, typeOfDataType (dataCol ^. dataColType))
        ColumnReport _ ->
          Nothing
  pure $ Map.fromList $ mapMaybe goCol cols
