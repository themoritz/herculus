{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Typecheck
 ( typecheck
 ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Text            (Text)

import           Database.MongoDB     ((=:))

import           Lib.Expression
import           Lib.Model
import           Lib.Model.Types
import           Lib.Model.Column
import           Lib.Types

import           Monads

newtype TypecheckT m a = TypecheckT
  { unTypecheckT :: ReaderT (Id Table) (ExceptT Text m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Id Table)
             , MonadError Text
             )

instance MonadTrans TypecheckT where
  lift = TypecheckT . lift . lift

typecheck :: MonadHexl m => Id Table -> Expr -> DataType -> m (Either Text ATExpr)
typecheck tblId expr signature = do
  let action = unTypecheckT $ typecheck' expr
      go atexpr@(_ ::: ttype) = case checkSig signature ttype of
        Just Ok -> Right atexpr
        Nothing -> Left "expression does not match type signature"
  (>>= go) <$> (runExceptT $ runReaderT action tblId)

typecheck' :: MonadHexl m => Expr -> TypecheckT m ATExpr
typecheck' expr = case expr of
  ExprColumnRef colRef -> do
    tblId <- ask
    (Entity i col) <- resolveColumnRef tblId colRef
    case columnDataType col of
      DataString -> pure $ TExprColumnRefString i ::: TypeString
      DataNumber -> pure $ TExprColumnRefNumber i ::: TypeNumber
      _          -> throwError "unsupported data type"
  ExprWholeColumnRef tblRef colRef -> do
    (Entity i col) <- resolveTableColumnRef tblRef colRef
    case columnDataType col of
      DataString -> pure $ TExprColumnRefStrings i ::: TypeStringList
      DataNumber -> pure $ TExprColumnRefNumbers i ::: TypeNumberList
      _          -> throwError "unsupported data type"
  ExprBinOp op lhs rhs -> do
    checkedL <- typecheck' lhs
    checkedR <- typecheck' rhs
    case (op, checkedL, checkedR) of
      (Append, l ::: TypeString, r ::: TypeString) -> pure $ TExprStringAppend l r ::: TypeString
      (Add,    l ::: TypeNumber, r ::: TypeNumber) -> pure $ TExprNumberAdd l r ::: TypeNumber
      _ -> throwError "not well typed binary operator"
  ExprUnOp Sum sub -> do
    checked <- typecheck' sub
    case checked of
      (s ::: TypeNumberList) -> pure $ TExprSum s ::: TypeNumber
      _ -> throwError "can only sum list of numbers"
  ExprStringLit s -> pure $ TExprLitString s ::: TypeString
  ExprNumberLit n -> pure $ TExprLitNumber n ::: TypeNumber


resolveColumnRef :: MonadHexl m => Id Table -> Ref Column -> TypecheckT m (Entity Column)
resolveColumnRef tbl colName = do
  let colQuery =
        [ "name" =: colName
        , "tableId" =: toObjectId tbl
        ]
  columnRes <- lift $ getOneByQuery colQuery
  case columnRes of
    Left _  -> throwError "column not found"
    Right e -> pure e

resolveTableColumnRef :: MonadHexl m => Ref Table -> Ref Column -> TypecheckT m (Entity Column)
resolveTableColumnRef tblName colName = do
  tableRes <- lift $ getOneByQuery [ "name" =: tblName ]
  case tableRes of
    Left _ -> throwError "table not found"
    Right e -> resolveColumnRef (entityId e) colName
