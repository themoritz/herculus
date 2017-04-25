{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |

module Engine.Compile
  ( compileColumn
  ) where

import           Lib.Prelude

import           Control.Lens

import           Data.Maybe               (mapMaybe)

import           Lib.Compiler
import           Lib.Compiler.Check.Monad (ResolveF (..), Resolver)
import           Lib.Compiler.Core
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Pretty
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
  let abort :: Text -> m (CompileResult a)
      abort msg = do
        void $ graphSetCodeDependencies columnId mempty
        pure $ CompileResultError msg
  case col ^. columnKind of
    ColumnData dataCol -> do
      res <- compileFormula
        (dataCol ^. dataColSourceCode)
        (resolver $ col ^. columnTableId)
        preludeCheckEnv
      compileResult <- case res of
        Left err -> abort $ errMsg err
        Right (expr, inferredType) -> do
          let colType = typeOfDataType (dataCol ^. dataColType)
          if inferredType /= colType
            then abort $
                   "Inferred type `" <> prettyType inferredType <>
                   "` does not match column type `" <>
                   prettyType colType <> "`."
            else do
              let deps = collectCodeDependencies expr
              cycles <- graphSetCodeDependencies columnId deps
              if cycles then abort "Dependency graph has cycles."
                        else pure $ CompileResultOk expr
      modifyColumn columnId $
        columnKind . _ColumnData . dataColCompileResult .~ compileResult
    ColumnReport repCol -> do
      res <- compileTemplate
        (repCol ^. reportColTemplate)
        (resolver $ col ^. columnTableId)
        preludeCheckEnv
      compileResult <- case res of
        Left err -> abort $ errMsg err
        Right tTpl -> do
          let deps = collectTplCodeDependencies tTpl
          cycles <- graphSetCodeDependencies columnId deps
          when cycles $
            throwError $ ErrBug "Setting report dependencies generated cycle"
          pure $ CompileResultOk tTpl
      modifyColumn columnId $
        columnKind . _ColumnReport . reportColCompiledTemplate .~ compileResult

--------------------------------------------------------------------------------

resolver :: MonadEngine m => Id Table -> Resolver m
resolver ownTblId = \case
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
