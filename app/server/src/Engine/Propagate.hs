{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Propagate
  ( propagate
  ) where

import           Lib.Prelude

import           Control.Lens                 hiding (children)

import qualified Data.HashMap.Strict          as HashMap

import           Lib.Api.Schema.Compiler      (moduleToCheckResult)
import           Lib.Compiler
import           Lib.Compiler.Check.Types     (resultCore)
import           Lib.Compiler.Eval.Types
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Common
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Types

import           Engine.Monad
import           Engine.Util
import           Monads

propagate :: MonadEngine m => m ()
propagate = do
  startCols <- getEvalRoots
  graphGetsM (getDependantsTopological getColumnTableId startCols) >>= \case
    Nothing ->
      throwError $ ErrBug $ "propagate: Dependency graph contains cycles."
    Just order -> do
      res <- useProject projectModule
      termEnv <- case res of
        CompileResultOk modu ->
          pure $ HashMap.union
            preludeTermEnv (loadModule $ resultCore $ moduleToCheckResult modu)
        CompileResultNone ->
          pure preludeTermEnv
        CompileResultError _ ->
          throwError $ ErrBug "propagate: Project module contains errors"
      propagate' termEnv order

propagate' :: forall m. MonadEngine m => TermEnv m -> ColumnOrder -> m ()
propagate' _ [] = pure ()
propagate' termEnv ((nextId, children):rest) = do
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
                evalFormula expr (mkGetter r) termEnv >>= \case
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

  propagate' termEnv rest

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
