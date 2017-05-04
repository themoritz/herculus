{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
-- |

module Engine.Util where

import           Lib.Prelude

import           Control.Lens            hiding (Getter)

import           Lib.Compiler.Eval.Monad (GetF (..), Getter)
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Engine.Monad
import           Monads

mkGetter :: MonadEngine m => Id Row -> Getter m
mkGetter r = \case
  GetCellValue c reply ->
    reply <$> getCellValue c r
  GetColumnValues c reply ->
    reply <$> getColumnValues c
  GetTableRows t reply ->
    map (reply . map entityId) $ getTableRows t
  GetRowRecord r' reply ->
    reply <$> getRowRecord r'

--------------------------------------------------------------------------------

graphGets :: MonadEngine m => (DependencyGraph -> a) -> m a
graphGets f = graphGetsM (pure . f)

getColumnTableId :: MonadEngine m => Id Column -> m (Id Table)
getColumnTableId c = view columnTableId <$> getColumn c

getCellValue :: MonadEngine m => Id Column -> Id Row -> m (Maybe Value)
getCellValue columnId rowId = do
  Entity _ cell <- getCellByCoord columnId rowId
  pure $ case cell ^. cellContent of
    CellError _ -> Nothing
    CellValue v -> Just v

getColumnValues :: MonadEngine m => Id Column -> m [Maybe Value]
getColumnValues columnId = map toMaybe <$> getColumnCells columnId
  where toMaybe (Entity _ (Cell content _ _ _)) = case content of
          CellError _ -> Nothing
          CellValue v -> Just v

withDataCol :: MonadError AppError m => Column -> (DataCol -> m a) -> m a
withDataCol col f = case col ^? columnKind . _ColumnData of
  Nothing      -> throwError $ ErrBug "withDataCol failed"
  Just dataCol -> f dataCol

withReportCol :: MonadError AppError m => Column -> (ReportCol -> m a) -> m a
withReportCol col f = case col ^? columnKind . _ColumnReport of
  Nothing        -> throwError $ ErrBug "withReportCol failed"
  Just reportCol -> f reportCol
