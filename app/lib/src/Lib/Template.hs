{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Lib.Template where

import           Lib.Prelude

import           Lib.Compiler
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Error
import           Lib.Compiler.Eval.Monad
import           Lib.Compiler.Eval.Types
import           Lib.Compiler.Parse
import           Lib.Template.Check
import           Lib.Template.Core
import qualified Lib.Template.Eval        as T
import           Lib.Template.Parse

compileTemplate
  :: Monad m => Text -> Resolver m -> CheckEnv
  -> m (Either Error [TplChunk])
compileTemplate src resolver env = runExceptT $ do
  e <- hoistError $ parse src (mkCheckEnvOpTable env) parseTemplate
  ExceptT $ runCheck env resolver $ checkTemplate e

evalTemplate
  :: Monad m => [TplChunk] -> Getter m -> TermEnv m
  -> m (Either Text Text)
evalTemplate tpl getter env = runEval 5000 getter $ T.evalTemplate env tpl

--------------------------------------------------------------------------------

testEvalTemplate :: Text -> IO ()
testEvalTemplate src =
  compileTemplate src testResolveInterp preludeCheckEnv >>= \case
    Left err -> putStrLn $ displayError src err
    Right code -> do
      evalTemplate code testGetInterp preludeTermEnv >>= \case
        Left err -> putStrLn err
        Right t -> putStrLn t
