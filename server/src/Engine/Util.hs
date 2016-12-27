{-# LANGUAGE FlexibleContexts #-}
-- |

module Engine.Util where

import           Control.Lens
import           Control.Monad.Except

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Types

import           Engine.Monad
import           Monads

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
