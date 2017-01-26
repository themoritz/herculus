{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |

module Engine.Compile
  ( compileColumn
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except

import           Data.Maybe                     (mapMaybe)
import           Data.Monoid
import           Data.Text                      (Text, pack)

import           Lib.Compiler
import           Lib.Compiler.Typechecker.Prim
import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Types
import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Template
import           Lib.Template.Types
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
      res <- compile (dataCol ^. dataColSourceCode)
                     (mkTypecheckEnv $ col ^. columnTableId)
      compileResult <- case res of
        Left msg -> abort msg
        Right (expr, inferredType) -> do
          colType <- typeOfDataType getTableRowType (dataCol ^. dataColType)
          if inferredType /= colType
            then abort $ pack $
                   "Inferred type `" <> show inferredType <>
                   "` does not match column type `" <>
                   show colType <> "`."
            else do
              let deps = collectCodeDependencies expr
              cycles <- graphSetCodeDependencies columnId deps
              if cycles then abort "Dependency graph has cycles."
                        else pure $ CompileResultOk expr
      modifyColumn columnId $
        columnKind . _ColumnData . dataColCompileResult .~ compileResult
    ColumnReport repCol -> do
      res <- compileTemplate (repCol ^. reportColTemplate)
                             (mkTypecheckEnv $ col ^. columnTableId)
      compileResult <- case res of
        Left msg -> abort msg
        Right tTpl -> do
          let deps = collectTplCodeDependencies tTpl
          cycles <- graphSetCodeDependencies columnId deps
          when cycles $
            throwError $ ErrBug "Setting report dependencies generated cycle"
          pure $ CompileResultOk tTpl
      modifyColumn columnId $
        columnKind . _ColumnReport . reportColCompiledTemplate .~ compileResult

--------------------------------------------------------------------------------

mkTypecheckEnv :: forall m. MonadEngine m => Id Table -> TypecheckEnv m
mkTypecheckEnv ownTblId = TypecheckEnv
    { envResolveColumnRef = \cRef ->
        (fmap.fmap) (\(_,i,c) -> (i,c)) $ resolveColumnRef ownTblId cRef

    , envResolveColumnOfTableRef = \tblName colName -> do
        tableRes <- getTableByName tblName
        case tableRes of
          Nothing           -> pure Nothing
          Just (Entity i _) -> resolveColumnRef i colName

    , envResolveTableRef = \tblName -> do
        tableRes <- getTableByName tblName
        case tableRes of
          Nothing               -> pure Nothing
          Just (Entity tblId _) -> pure $ Just tblId

    , envGetTableRowType = getTableRowType

    , envOwnTableId = ownTblId

    }
  where
    resolveColumnRef :: Id Table -> Ref Column
                     -> m (Maybe (Id Table, Id Column, DataCol))
    resolveColumnRef tableId colName = do
      columnRes <- getColumnOfTableByName tableId colName
      pure $ case columnRes of
        Nothing             -> Nothing
        Just (Entity i col) -> fmap (tableId,i,)
                                     (col ^? columnKind . _ColumnData)

getTableRowType :: MonadEngine m => Id Table -> m Type
getTableRowType t = do
  cols <- getTableColumns t
  let toRow [] = pure $ Type TyRecordNil
      toRow ((name, col):rest) = Type
        <$> (TyRecordCons (Ref name)
        <$> typeOfDataType getTableRowType (col ^. dataColType)
        <*> toRow rest)
  toRow $ flip mapMaybe cols $ \(Entity _ col) ->
    fmap (col ^. columnName,) (col ^? columnKind . _ColumnData)
