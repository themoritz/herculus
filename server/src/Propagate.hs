{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Propagate
  ( runPropagate
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

import           CellCache
import           Eval
import           Lib.Types
import           Lib.Model.Types
import           Monads
import           Lib.Expression
import           Lib.Api.WebSocket

data PropEnv = PropEnv
  { propRecordId :: Id Record
  }

newtype PropT m a = PropT
  { unPropT :: ReaderT PropEnv (StateT CellCache m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader PropEnv
             , MonadState CellCache
             )

instance MonadTrans PropT where
  lift = PropT . lift  . lift

runPropagate :: MonadHexl m => Id Record -> [Id Column] -> m ()
runPropagate recId order = do
  let action = unPropT $ propagate order
      env = PropEnv recId
  cache <- execStateT (runReaderT action env) emptyCellCache
  sendWS $ WsDownCellsChanged $ retrieveAll cache

propagate :: MonadHexl m => [Id Column] -> PropT m ()
propagate [] = pure ()
propagate (colId:cols) = do
  expr <- lift $ getExpression colId
  cache <- get
  recId <- asks propRecordId
  lift (runEval cache recId expr) >>= \case
    Left _ -> propagate cols
    Right val -> do
      modify $ store colId recId val
      propagate cols

getExpression :: MonadHexl m => Id Column -> m Expr
getExpression colId = do
  col <- getById' colId
  case columnType col of
    ColumnDerived formula -> case parseExpression formula of
      Right expr -> pure expr
      Left _ -> throwError $ ErrBug "cannot parse stored expression"
    _ -> pure (ExprStringLit "")
