{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Propagate
  ( propagate
  ) where

import           Data.Foldable

import           Eval
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Types
import           Lib.Types
import           Monads

import           Propagate.Monad

propagate :: MonadHexl m => Id Column -> Propagate -> m ()
propagate c start = do
  order <- getColumnOrder c

  runPropagate $ do
    addTargets c start
    propagate' order

propagate' :: forall m. MonadPropagate m => ColumnOrder -> m ()
propagate' [] = pure ()
propagate' ((next, children):rest) = do
  records <- getTargets next
  (texpr ::: _) <- getCompiledCode next

  let doTarget :: Id Record -> m ()
      doTarget r = do

        let env = EvalEnv
                    { envGetCellValue = flip getCellValue r
                    , envGetColumnValues = getColumnValues
                    }

        result <- eval env texpr
        setCellResult next r result

        for_ children $ \(child, depType) -> case depType of
          OneToOne -> addTargets child (OneRecord r)
          OneToAll -> addTargets child CompleteColumn

  mapM_ doTarget records

  propagate' rest
