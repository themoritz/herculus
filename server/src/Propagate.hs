{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Propagate
  ( PropagationRoot (..)
  , propagate
  ) where

import           Control.Monad.Except

import           Data.Foldable

import           Eval
import           Lib.Model.Column
import           Lib.Model.Cell
import           Lib.Model.Dependencies
import           Lib.Model.Types
import           Lib.Types
import           Monads

import           Propagate.Monad

data PropagationRoot
  = RootCellChanges [(Id Column, Id Record)]
  | RootWholeColumns [Id Column]

propagate :: MonadHexl m => PropagationRoot -> m ()
propagate root = runPropagate $ case root of
  RootWholeColumns cs -> do
    for_ cs $ \c -> addTargets c CompleteColumn
    order <- lift $ getColumnOrder cs
    propagate' order
  RootCellChanges coords -> do
    graph <- lift getDependencies
    for_ coords $ \(c, r) ->
      for_ (getChildren c graph) $ \(child, depType) -> case depType of
        OneToOne -> addTargets child (OneRecord r)
        OneToAll -> addTargets child CompleteColumn
    order <- lift $ getColumnOrder $ map fst coords
    -- Don't need to remove the root columns because they have no targets
    propagate' order

propagate' :: forall m. MonadPropagate m => ColumnOrder -> m ()
propagate' [] = pure ()
propagate' ((next, children):rest) = do
  records <- getTargets next
  compileResult <- getCompileResult next

  let doTarget :: Id Record -> m ()
      doTarget r = do
        result <- case compileResult of
          CompileResultCode (texpr ::: _) -> do
            let env = EvalEnv
                        { envGetCellValue = flip getCellValue r
                        , envGetColumnValues = getColumnValues
                        }
            eval env texpr
          CompileResultError _ -> pure $
            CellEvalError "Column not compiled"
          CompileResultNone -> throwError $
            ErrBug "propagate: no compile result for column"

        setCellContent next r result

        for_ children $ \(child, depType) -> case depType of
          OneToOne -> addTargets child (OneRecord r)
          OneToAll -> addTargets child CompleteColumn

  mapM_ doTarget records

  propagate' rest
