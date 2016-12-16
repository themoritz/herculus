{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Propagate
  ( PropagationRoot (..)
  , propagate
  ) where

import           Control.Lens                   hiding (children)
import           Control.Monad.Except
import           Control.Monad.State            (StateT, evalStateT)

import           Data.Foldable
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Traversable

import           Database.MongoDB               ((=:))

import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Engine.Monad
import           Monads

data PropagationRoot
  = RootCellChanges [(Id Table, Id Column, Id Row)]
  | RootWholeColumns [Id Column]
  | RootSpecificCells [(Id Column, Id Row)]

propagate :: MonadEngine db m => [PropagationRoot] -> m ()
propagate roots = flip evalStateT Map.empty $ do
  startCols <- for roots $ \root -> case root of
    RootCellChanges coords -> do
      for_ coords $ \(t, c, r) -> do
        childs <- lift $ graphGets $ getAllColumnDependants (c, t)
        for_ childs $ \(child, mode) ->
          case mode of
            AddOne -> addTargets child (OneRow r)
            AddAll -> addTargets child CompleteColumn
      -- Don't need to remove the root columns because they have no targets
      pure $ map (view _2) coords
    RootWholeColumns cs -> do
      for_ cs $ \c -> addTargets c CompleteColumn
      pure cs
    RootSpecificCells coords -> do
      for_ coords $ \(c, r) -> addTargets c (OneRow r)
      pure $ map fst coords
  lift (graphGets (getDependantsTopological (join startCols))) >>= \case
    Nothing -> throwError $ ErrBug "propagate: Dependency graph contains cycles. Please report this as a bug!"
    Just order -> propagate' order

propagate' :: forall db m. MonadEngine db m => ColumnOrder -> PropT m ()
propagate' [] = pure ()
propagate' ((next, children):rest) = do
  let hop r = for_ children $ \(child, mode) -> case mode of
        AddOne -> addTargets child (OneRow r)
        AddAll -> addTargets child CompleteColumn

  rows <- getTargets next
  whatToDoWithCells next >>= \case

    Evaluate compileResult -> do
      let doTarget :: Id Row -> PropT m ()
          doTarget r = do
            result <- case compileResult of
              CompileResultOk expr -> do
                let env = EvalEnv
                            { envGetCellValue    = \c -> lift $ getCellValue c r
                            , envGetColumnValues = lift . getColumnValues
                            , envGetTableRows    = lift . getTableRows
                            , envGetRowField     = \r' c -> lift $ getRowField r' c
                            }
                interpret expr env >>= \case
                  Left e -> pure $ CellError e
                  Right v -> pure $ CellValue v
              CompileResultError _ -> pure $
                CellError "Column not compiled"
              CompileResultNone -> throwError $
                ErrBug "propagate: no compile result for column"

            lift $ setCellContent next r result
            hop r
      mapM_ doTarget rows

    Hop -> mapM_ hop rows

    DoNothing -> pure ()

  propagate' rest

--------------------------------------------------------------------------------

data AddTarget
  = CompleteColumn
  | OneRow (Id Row)

data CellUpdateAction
  = Evaluate DataCompileResult
  -- ^ Formula, so evaluate
  | Hop
  -- ^ No formula, hop because of references
  | DoNothing
  -- ^ For report columns

type PropState = Map (Id Column) (Maybe [Id Row]) -- Nothing = All

type PropT m = StateT PropState m

addTargets :: MonadEngine db m => Id Column -> AddTarget -> PropT m ()
addTargets c prop = at c . non (Just []) %=
  \col -> case (col, prop) of
    (Just rs, OneRow r) -> Just $ r:rs
    _                   -> Nothing

getTargets :: MonadEngine db m => Id Column -> PropT m [Id Row]
getTargets c = use (at c . non (Just [])) >>= \case
  Just rs -> pure rs
  Nothing -> do
    col <- lift $ getColumn c
    rows <- lift $ liftDB $ listByQuery
      [ "tableId" =: toObjectId (_columnTableId col) ]
    pure $ map entityId rows

whatToDoWithCells :: MonadEngine db m => Id Column -> PropT m CellUpdateAction
whatToDoWithCells c = do
  col <- lift $ getColumn c
  case col ^. columnKind of
    ColumnReport _ -> pure DoNothing
    ColumnData dataCol -> case dataCol ^. dataColIsDerived of
      Derived    -> pure $ Evaluate (dataCol ^. dataColCompileResult)
      NotDerived -> pure $ Hop
