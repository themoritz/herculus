{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Template where

import           Lib.Prelude

import           Lib.Compiler
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Eval.Monad
import           Lib.Template.Check
import           Lib.Template.Eval
import           Lib.Template.Parse

-- compileTemplate :: Monad m => Text -> TypecheckEnv m -> m (Either Text CTemplate)
-- compileTemplate inp env = case parseTemplate inp of
--   Left e -> pure $ Left e
--   Right tpl -> do
--     inferResult <- runInferTpl env tpl
--     case inferResult of
--       Left e -> pure $ Left e
--       Right ttpl -> pure $ Right ttpl

testEvalTemplate :: Text -> IO ()
testEvalTemplate src = withParsed src parseTemplate $ \tpl -> do
  let go = checkTemplate tpl >>= compileTemplate
  runCheck primCheckEnv testResolveInterp go >>= \case
    Left err -> putStrLn $ displayError src err
    Right code -> do
      runEval 10000 testGetInterp (evalTemplate primTermEnv code) >>= \case
        Left err -> putStrLn err
        Right t -> putStrLn t
