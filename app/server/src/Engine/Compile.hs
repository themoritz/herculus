{-# LANGUAGE NoImplicitPrelude   #-}
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

import           Data.Maybe                (mapMaybe)

import           Lib.Compiler
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Check.Monad  (ResolveF (..), Resolver)
import           Lib.Compiler.Core
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Type
import           Lib.Model
import           Lib.Model.Column
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

checkDataCol
  :: MonadEngine m
  => Id Table -> Id Column -> DataType -> Text -> m DataCompileResult
checkDataCol t c dt src = do
  let colType = typeOfDataType dt
  res <- compileFormulaWithType src colType (mkResolver t) preludeCheckEnv
  case res of
    Left err -> abort c [err]
    Right expr -> do
      let deps = collectCodeDependencies expr
      cycles <- graphSetCodeDependencies c deps
      if cycles then abort c [Error "Dependency graph has cycles." voidSpan]
                else pure $ CompileResultOk expr

checkReportCol
  :: MonadEngine m
  => Id Table -> Id Column -> Text -> m ReportCompileResult
checkReportCol t c src = do
  res <- compileTemplate src (mkResolver t) preludeCheckEnv
  case res of
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

getTableRecordType :: MonadEngine m => Id Table -> m Type
getTableRecordType t = do
  cols <- getTableColumns t
  pure $ typeApp tyRecord $ go $ flip mapMaybe cols $ \(Entity _ col) ->
    fmap (col ^. columnName,) (col ^? columnKind . _ColumnData)
  where
  go [] = recordNil
  go ((name, col):rest) =
    recordCons name (typeOfDataType (col ^. dataColType)) (go rest)
