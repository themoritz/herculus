{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Propagate
  ( runPropagate
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

import           Data.Monoid

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
    Left msg -> lift $ throwError $ ErrBug $ "error during eval: " <> msg
    Right val -> do
      modify $ store colId recId val
      lift $ setCellByCoords colId recId val
      propagate cols

getExpression :: MonadHexl m => Id Column -> m Expr
getExpression colId = do
  col <- getById' colId
  case columnType col of
    ColumnDerived formula -> case parseExpression formula of
      Right expr -> pure expr
      Left _ -> throwError $ ErrBug "cannot parse stored expression"
    _ -> throwError $ ErrBug "column is not derived"

setCellByCoords :: MonadHexl m => Id Column -> Id Record -> Value -> m ()
setCellByCoords c r v = do
  (Column t _ _) <- getById' c
  upsertCell (Cell v (Aspects t c r))
