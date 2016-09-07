{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Propagate
  ( PropagationRoot (..)
  , propagate
  ) where

import           Control.Monad.Except

import           Data.Foldable
import           Data.Traversable

import           Lib.Api.WebSocket
import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Record
import           Lib.Types

import           Cache
import           Monads
import           Propagate.Monad

data PropagationRoot
  = RootCellChanges [(Id Column, Id Record)]
  | RootWholeColumns [Id Column]
  | RootSpecificCells [(Id Column, Id Record)]

propagate :: MonadHexl m => [PropagationRoot] -> m ()
propagate roots = do
  (_, changedCells) <- runPropT $ do
    startCols <- for roots $ \root -> case root of
      RootCellChanges coords -> do
        graph <- lift getDependencies
        for_ coords $ \(c, r) ->
          for_ (getChildren c graph) $ \(child, depType) -> case depType of
            OneToOne -> addTargets child (OneRecord r)
            OneToAll -> addTargets child CompleteColumn
        -- Don't need to remove the root columns because they have no targets
        pure $ map fst coords
      RootWholeColumns cs -> do
        for_ cs $ \c -> addTargets c CompleteColumn
        pure cs
      RootSpecificCells coords -> do
        for_ coords $ \(c, r) -> addTargets c (OneRecord r)
        pure $ map fst coords
    order <- lift $ getColumnOrder $ join startCols
    propagate' order
  sendWS $ WsDownCellsChanged changedCells

propagate' :: forall m. MonadPropagate m => ColumnOrder -> m ()
propagate' [] = pure ()
propagate' ((next, children):rest) = do
  records <- getTargets next
  getCompileResult next >>= \case
    Nothing -> pure () -- Nothing means that the column is not a data column
    Just compileResult -> do
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

            for_ children $ \(child, depType) -> case depType of
              OneToOne -> addTargets child (OneRecord r)
              OneToAll -> addTargets child CompleteColumn

      mapM_ doTarget records

  propagate' rest
