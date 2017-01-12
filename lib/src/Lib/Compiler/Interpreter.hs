{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler.Interpreter where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text, pack)
import           Data.Traversable

import           Lib.Model.Cell
import           Lib.Types

import           Lib.Compiler.Interpreter.Types
import           Lib.Compiler.Types

-- Helper

classFunction :: Monad m => Name -> (Name, Result m)
classFunction name =
  ( name
  , RPrelude $ \_ (RInstanceDict d) -> let Just f = Map.lookup name d in pure f
  )

app3Var :: Name -> Name -> Name -> Name -> CExpr
app3Var f a b c = CApp (CApp (CApp (CVar f) (CVar a)) (CVar b)) (CVar c)

app4Var :: Name -> Name -> Name -> Name -> Name -> CExpr
app4Var f a b c d = CApp (CApp (CApp (CApp (CVar f) (CVar a)) (CVar b)) (CVar c)) (CVar d)

inTermsOfEq :: Monad m => (Bool -> Bool) -> Result m
inTermsOfEq f =
  RPrelude $ \env dict -> pure $ RPrelude $ \_ a -> pure $ RPrelude $ \_ b -> do
    RValue (VBool eq) <- eval (Map.insert "dict" dict $ Map.insert "a" a $ Map.insert "b" b env)
                              (app3Var "==" "dict" "a" "b")
    pure $ RValue $ VBool $ f eq

inTermsOfEqOrd :: Monad m => (Bool -> Bool -> Bool) -> Result m
inTermsOfEqOrd f =
  RPrelude $ \env dictEq -> pure $ RPrelude $ \_ dictOrd -> pure $ RPrelude $ \_ a -> pure $ RPrelude $ \_ b -> do
    RValue (VBool eq) <- eval (Map.insert "dict" dictEq $ Map.insert "a" a $ Map.insert "b" b env)
                              (app3Var "==" "dict" "a" "b")
    RValue (VBool le) <- eval (Map.insert "dictEq" dictEq $ Map.insert "dictOrd" dictOrd $ Map.insert "a" a $ Map.insert "b" b env)
                              (app4Var "<=" "dictEq" "dictOrd" "a" "b")
    pure $ RValue $ VBool $ f eq le

-- Prelude

-- Idea: Use Higher Order Abstract Syntax (HOAS, see Phil Freeman talk) here?
prelude :: Monad m => Map Name (Result m)
prelude = Map.fromList
  [ ( "sum"
    , RPrelude $ \_ (RValue (VList xs)) -> pure $
        RValue $ VNumber $ sum $ map (\(VNumber v) -> v) xs
    )
  , ( "length"
    , RPrelude $ \_ (RValue (VList xs)) -> pure $ RValue $ VNumber $ fromIntegral $ length xs
    )
  , ( "const"
    , RPrelude $ \_ a -> pure $ RPrelude $ \_ _ -> pure a
    )
  , ( "not"
    , RPrelude $ \_ (RValue (VBool b)) -> pure $ RValue $ VBool $ not b
    )
  , ( "*"
    , RPrelude $ \_ (RValue (VNumber a)) -> pure $ RPrelude $ \_ (RValue (VNumber b)) ->
        pure $ RValue $ VNumber $ a * b
    )
  , ( "+"
    , RPrelude $ \_ (RValue (VNumber a)) -> pure $ RPrelude $ \_ (RValue (VNumber b)) ->
        pure $ RValue $ VNumber $ a + b
    )
  , ( "-"
    , RPrelude $ \_ (RValue (VNumber a)) -> pure $ RPrelude $ \_ (RValue (VNumber b)) ->
        pure $ RValue $ VNumber $ a - b
    )
  , ( "/"
    , RPrelude $ \_ (RValue (VNumber a)) -> pure $ RPrelude $ \_ (RValue (VNumber b)) ->
        if b == 0
          then throwError "Division by zero"
          else pure $ RValue $ VNumber $ a / b
    )
  -- Class Functor
  , classFunction "map"
  , ( "$FunctorList"
    , RInstanceDict $ Map.fromList
      [ ( "map"
        , RPrelude $ \env arg -> pure $ RPrelude $ \_ (RValue (VList xs)) -> do
            let f x = case arg of
                  RClosure name body cl -> do
                    RValue v <- eval (Map.insert name (RValue x) cl) body
                    pure v
                  RPrelude f' -> do
                    RValue v <- f' env (RValue x)
                    pure v
                  _ -> throwError "Prelude `map`: pattern match failure. Please report this as a bug!"
            RValue . VList <$> traverse f xs
        )
      ]
    )
  , ( "$FunctorMaybe"
    , RInstanceDict $ Map.fromList
      [ ( "map"
        , RPrelude $ \env arg -> pure $ RPrelude $ \_ (RValue (VMaybe may)) -> do
            let f x = case arg of
                  RClosure name body cl -> do
                    RValue v <- eval (Map.insert name (RValue x) cl) body
                    pure v
                  RPrelude f' -> do
                    RValue v <- f' env (RValue x)
                    pure v
                  _ -> throwError "Prelude `map`: pattern match failure. Please report this as a bug!"
            RValue . VMaybe <$> traverse f may
        )
      ]
    )
  -- Class Semigroup
  , classFunction "<>"
  , ( "$SemigroupString"
    , RInstanceDict $ Map.fromList
      [ ( "<>"
        , RPrelude $ \_ (RValue (VString a)) -> pure $ RPrelude $ \_ (RValue (VString b)) ->
            pure $ RValue (VString (a <> b))
        )
      ]
    )
  , ( "$SemigroupList"
    , RInstanceDict $ Map.fromList
      [ ( "<>"
        , RPrelude $ \_ (RValue (VList a)) -> pure $ RPrelude $ \_ (RValue (VList b)) ->
            pure $ RValue (VList (a <> b))
        )
      ]
    )
  -- Class show
  , classFunction "show"
  , ( "$ShowNumber"
    , RInstanceDict $ Map.fromList
      [ ( "show"
        , RPrelude $ \_ (RValue (VNumber n)) -> pure $ RValue $ VString $ pack $ show n
        )
      ]
    )
  , ( "$ShowBool"
    , RInstanceDict $ Map.fromList
      [ ( "show"
        , RPrelude $ \_ (RValue (VBool b)) -> pure $ RValue $ VString $ pack $ show b
        )
      ]
    )
  -- Class Eq
  , classFunction "=="
  , ( "!="
    , inTermsOfEq not
    )
  -- Instances Eq
  , ( "$EqNumber"
    , RInstanceDict $ Map.fromList
      [ ( "=="
        , RPrelude $ \_ (RValue (VNumber a)) -> pure $ RPrelude $ \_ (RValue (VNumber b)) ->
            pure $ RValue $ VBool $ a == b
        )
      ]
    )
  , ( "$EqBool"
    , RInstanceDict $ Map.fromList
      [ ( "=="
        , RPrelude $ \_ (RValue (VBool a)) -> pure $ RPrelude $ \_ (RValue (VBool b)) ->
            pure $ RValue $ VBool $ a == b
        )
      ]
    )
  , ( "$EqTime"
    , RInstanceDict $ Map.fromList
      [ ( "=="
        , RPrelude $ \_ (RValue (VTime a)) -> pure $ RPrelude $ \_ (RValue (VTime b)) ->
            pure $ RValue $ VBool $ a == b
        )
      ]
    )
  , ( "$EqString"
    , RInstanceDict $ Map.fromList
      [ ( "=="
        , RPrelude $ \_ (RValue (VString a)) -> pure $ RPrelude $ \_ (RValue (VString b)) ->
            pure $ RValue $ VBool $ a == b
        )
      ]
    )
  -- Class Ord
  , ( "<="
    , RPrelude $ \_ _ -> pure $ RPrelude $ \_ (RInstanceDict dictOrd) -> -- first argument for dictEq, but not needed here
        let Just f = Map.lookup "<=" dictOrd in pure f
    )
  , ( ">="
    , inTermsOfEqOrd $ \eq le -> eq || not le
    )
  , ( "<"
    , inTermsOfEqOrd $ \eq le -> not eq && le
    )
  , ( ">"
    , inTermsOfEqOrd $ \_ le -> not le
    )
  -- Instances Ord
  , ( "$OrdNumber"
    , RInstanceDict $ Map.fromList
      [ ( "<="
        , RPrelude $ \_ (RValue (VNumber a)) -> pure $ RPrelude $ \_ (RValue (VNumber b)) ->
            pure $ RValue $ VBool $ a <= b
        )
      ]
    )
  , ( "$OrdTime"
    , RInstanceDict $ Map.fromList
      [ ( "<="
        , RPrelude $ \_ (RValue (VTime a)) -> pure $ RPrelude $ \_ (RValue (VTime b)) ->
            pure $ RValue $ VBool $ a <= b
        )
      ]
    )
  --
  , ( "||"
    , RPrelude $ \_ (RValue (VBool a)) -> pure $ RPrelude $ \_ (RValue (VBool b)) ->
        pure $ RValue $ VBool $ a || b
    )
  , ( "&&"
    , RPrelude $ \_ (RValue (VBool a)) -> pure $ RPrelude $ \_ (RValue (VBool b)) ->
        pure $ RValue $ VBool $ a && b
    )
  --
  , ( "formatNumber"
    , RPrelude $ \_ (RValue (VString f)) -> pure $ RPrelude $ \_ (RValue (VNumber n)) ->
        pure $ RValue $ VString $ formatNumber f n
    )
  , ( "formatTime"
    , RPrelude $ \_ (RValue (VString f)) -> pure $ RPrelude $ \_ (RValue (VTime t)) ->
        pure $ RValue $ VString $ formatTime f t
    )
  , ( "filter"
    , RPrelude $ \env arg -> pure $ RPrelude $ \_ (RValue (VList xs)) -> do
        let p x = case arg of
              RClosure name body cl -> do
                RValue (VBool b) <- eval (Map.insert name (RValue x) cl) body
                pure b
              RPrelude f' -> do
                RValue (VBool b) <- f' env (RValue x)
                pure b
              _ -> throwError "Prelude `filter`: pattern match failure. Please report this as a bug!"
        RValue . VList <$> filterM p xs
    )
  , ( "find"
    , RPrelude $ \env arg -> pure $ RPrelude $ \_ (RValue (VList xs)) -> do
        let p x = case arg of
              RClosure name body cl -> do
                RValue (VBool b) <- eval (Map.insert name (RValue x) cl) body
                pure b
              RPrelude f' -> do
                RValue (VBool b) <- f' env (RValue x)
                pure b
              _ -> throwError "Prelude `find`: pattern match failure. Please report this as a bug!"
            findM _ [] = pure Nothing
            findM p' (x:xs') = do
              b <- p' x
              if b then pure (Just x) else findM p' xs'
        RValue . VMaybe <$> findM p xs
    )
  , ( "fromMaybe"
    , RPrelude $ \_ (RValue def) -> pure $ RPrelude $ \_ (RValue (VMaybe may)) ->
        pure $ RValue $ fromMaybe def may
    )
  , ( "maybe"
    , RPrelude $ \_ (RValue b) -> pure $ RPrelude $ \env farg -> pure $ RPrelude $ \_ (RValue (VMaybe may)) -> do
        let f a' = case farg of
              RClosure name body cl -> do
                RValue b' <- eval (Map.insert name (RValue a') cl) body
                pure b'
              RPrelude f' -> do
                RValue b' <- f' env (RValue a')
                pure b'
              _ -> throwError "Prelude `maybe`: pattern match failure. Please report this as a bug!"
        case may of
          Nothing -> pure $ RValue b
          Just a  -> RValue <$> f a
    )
  ]

-- Interpreter

consumeGas :: Monad m => InterpretT m ()
consumeGas = do
  gas <- get
  when (gas == 0) $ throwError "Computation exceeded the maximum allowed number of operations."
  put (gas - 1)

-- TODO: More refined model of when gas is consumed might be needed
eval :: Monad m => TermEnv m -> CExpr -> InterpretT m (Result m)
eval env expr = consumeGas >> case expr of
  CLam x body -> pure $ RClosure x body env
  CApp f arg -> do
    argVal <- eval env arg
    eval env f >>= \case
      RClosure x body cl -> eval (Map.insert x argVal cl) body
      RPrelude f' -> f' env argVal
      _ -> throwError "Interpreter `App`: pattern match failure. Please report this as a bug!"
  CLet x e body -> do
    eVal <- eval env e
    eval (Map.insert x eVal env) body
  CIf cond e1 e2 -> do
    RValue (VBool bVal) <- eval env cond
    if bVal then eval env e1 else eval env e2
  CVar x -> case Map.lookup x env of
    Nothing -> throwError $ "Interpreter `Var`: " <> (pack . show) x <> " not found in env. Please report this as a bug!"
    Just v -> pure v
  CLit l -> do
    let v = case l of
          LNumber v' -> VNumber v'
          LBool v'   -> VBool v'
          LString v' -> VString v'
    pure $ RValue v
  CPrjRecord e name -> do
    RValue (VRowRef mRecId) <- eval env e
    case mRecId of
      Nothing -> throwError "Dependent cell not ready (invalid reference)"
      Just recId -> do
        f <- asks envGetRowField
        lift (f recId name) >>= \case
          Nothing -> throwError "Dependent cell not ready"
          Just val -> pure $ RValue val
  CColumnRef colId -> do
    f <- asks envGetCellValue
    lift (f colId) >>= \case
      Nothing -> throwError "Dependent cell not ready"
      Just val -> pure $ RValue val
  CWholeColumnRef _ colId -> do
    f <- asks envGetColumnValues
    mVals <- lift $ f colId
    fmap (RValue . VList) $ for mVals $ \case
      Nothing -> throwError "Dependent cell not ready"
      Just val -> pure val
  CTableRef tblId -> do
    f <- asks envGetTableRows
    rows <- lift $ f tblId
    pure $ RValue $ VList $ map (VRowRef . Just) rows

interpret :: Monad m => CExpr -> EvalEnv m -> m (Either Text Value)
interpret expr env = do
  result <- runInterpretT 100000 env (eval prelude expr)
  case result of
    Left e -> pure $ Left e
    Right (RValue v) -> pure $ Right v
    Right RClosure{} -> pure $ Left "Interpret result: Did not expect closure. Please report this as a bug!"
    Right RPrelude{} -> pure $ Left "Interpret result: Did not expect prelude function. Please report this as a bug!"
    _ -> pure $ Left "Interpret result: Pattern match failure. Please report this as a bug!"
