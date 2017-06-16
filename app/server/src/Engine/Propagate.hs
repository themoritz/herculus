{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Propagate
  ( propagate
  ) where

import           Lib.Prelude

import           Control.Lens                 hiding (children)

import qualified Data.Text                    as T

import           Lib.Compiler
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Common
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Row
import           Lib.Types

import           Engine.Monad
import           Engine.Util
import           Monads

propagate :: MonadEngine m => m ()
propagate = do
  startCols <- getEvalRoots
  graphGetsM (getDependantsTopological getColumnTableId startCols) >>= \case
    Nothing -> throwError $ ErrBug $ T.unlines
      [ "propagate: Dependency graph contains cycles."
      , "Please report this as a bug!" ]
    Just order -> propagate' order

propagate' :: forall m. MonadEngine m => ColumnOrder -> m ()
propagate' [] = pure ()
propagate' ((nextId, children):rest) = do
  let hop rowId = for_ children $ \(childId, mode) -> case mode of
        AddOne -> scheduleEvalCell childId rowId
        AddAll -> scheduleEvalColumn childId

  rows <- getEvalTargets nextId
  whatToDoWithCells nextId >>= \case

    Evaluate compileResult -> do
      let doTarget :: Id Row -> m ()
          doTarget r = do
            result <- case compileResult of
              CompileResultOk expr -> do
                evalFormula expr (mkGetter r) preludeTermEnv >>= \case
                  Left e -> pure $ CellError e
                  Right v -> pure $ CellValue v
              CompileResultError _ -> pure $
                CellError "Column not compiled"
              CompileResultNone -> throwError $
                ErrBug "propagate: no compile result for column"

            setCellContent nextId r result
            hop r
      mapM_ doTarget rows

    Hop -> mapM_ hop rows

    DoNothing -> pure ()

  propagate' rest

--------------------------------------------------------------------------------

data CellUpdateAction
  = Evaluate DataCompileResult
  -- ^ Formula, so evaluate
  | Hop
  -- ^ No formula, hop because of references
  | DoNothing
  -- ^ For report columns

whatToDoWithCells :: MonadEngine m => Id Column -> m CellUpdateAction
whatToDoWithCells c = do
  col <- getColumn c
  case col ^. columnKind of
    ColumnReport _ -> pure DoNothing
    ColumnData dataCol -> case dataCol ^. dataColIsDerived of
      Derived    -> pure $ Evaluate (dataCol ^. dataColCompileResult)
      NotDerived -> pure $ Hop
