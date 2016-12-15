{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Propagate
  ( PropagationRoot (..)
  , propagate
  ) where

import           Control.Lens                   hiding (children)
import           Control.Monad.Except

import           Data.Foldable
import           Data.Traversable

import           Lib.Api.WebSocket
import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

import           Cache
import           Monads
import           Propagate.Monad

data PropagationRoot
  = RootCellChanges [(Id Table, Id Column, Id Record)]
  | RootWholeColumns [Id Column]
  | RootSpecificCells [(Id Column, Id Record)]

propagate :: MonadHexl m => DependencyGraph -> [PropagationRoot] -> m ()
propagate graph roots = do
  (_, changedCells) <- runPropT $ do
    startCols <- for roots $ \root -> case root of
      RootCellChanges coords -> do
        for_ coords $ \(t, c, r) ->
          for_ (getAllColumnDependants (c, t) graph) $ \(child, mode) -> case mode of
            AddOne -> addTargets child (OneRecord r)
            AddAll -> addTargets child CompleteColumn
        -- Don't need to remove the root columns because they have no targets
        pure $ map (view _2) coords
      RootWholeColumns cs -> do
        for_ cs $ \c -> addTargets c CompleteColumn
        pure cs
      RootSpecificCells coords -> do
        for_ coords $ \(c, r) -> addTargets c (OneRecord r)
        pure $ map fst coords
    case getDependantsTopological (join startCols) graph of
      Nothing -> throwError $ ErrBug "propagate: Dependency graph contains cycles. Please report this as a bug!"
      Just order -> propagate' order
  upsertMany changedCells
  sendWS $ WsDownCellsChanged (map entityVal changedCells)

propagate' :: forall m. MonadPropagate m => ColumnOrder -> m ()
propagate' [] = pure ()
propagate' ((next, children):rest) = do
  let hop r = for_ children $ \(child, mode) -> case mode of
        AddOne -> addTargets child (OneRecord r)
        AddAll -> addTargets child CompleteColumn

  records <- getTargets next
  whatToDoWithCells next >>= \case

    Evaluate compileResult -> do
      let doTarget :: Id Record -> m ()
          doTarget r = do
            result <- case compileResult of
              CompileResultOk expr -> do
                let env = EvalEnv
                            { envGetCellValue = flip getCellValue r
                            , envGetColumnValues = getColumnValues
                            , envGetTableRecords = getTableRecords
                            , envGetRecordValue = getRecordValue
                            }
                interpret expr env >>= \case
                  Left e -> pure $ CellError e
                  Right v -> pure $ CellValue v
              CompileResultError _ -> pure $
                CellError "Column not compiled"
              CompileResultNone -> throwError $
                ErrBug "propagate: no compile result for column"

            setCellContent next r result
            hop r
      mapM_ doTarget records

    Hop -> mapM_ hop records

    DoNothing -> pure ()

  propagate' rest
