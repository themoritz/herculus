{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}

module Propagate
  ( runPropagate
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except

import           Data.Monoid
import           Data.Text (pack)

import           CellCache
import           Eval
import           Typecheck
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
  col <- lift $ getById' colId
  case parseExpression $ columnExpression col of
    Left msg -> lift $ throwError $ ErrBug $ "cannot parse stored expression: " <> pack (show msg)
    Right expr -> do
      cache <- get
      recId <- asks propRecordId
      lift (runTypecheck (columnTableId col) expr (columnType col)) >>= \case
        Left msg -> lift $ throwError $ ErrBug $ "type checker failed: " <> msg
        Right (texpr ::: _) -> lift (runEval cache recId texpr) >>= \case
          Left msg -> lift $ throwError $ ErrBug $ "error during eval: " <> msg
          Right val -> do
            modify $ store colId recId $ showValue val
            lift $ setCellByCoords colId recId $ showValue val
            propagate cols

setCellByCoords :: MonadHexl m => Id Column -> Id Record -> Value -> m ()
setCellByCoords c r v = do
  (Column t _ _ _ _) <- getById' c
  upsertCell (Cell v (Aspects t c r))
