{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Lib.Template where

import           Lib.Prelude

import           Lib.Compiler
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Eval.Monad
import           Lib.Compiler.Parse
import           Lib.Template.Check
import           Lib.Template.Core
import           Lib.Template.Eval
import           Lib.Template.Parse

compileTemplate
  :: Monad m => Text -> Resolver m -> CheckEnv
  -> m (Either Error [TplChunk])
compileTemplate src resolver env = runExceptT $ do
  e <- hoistError $ parse src parseTemplate
  ExceptT $ runCheck env resolver $ checkTemplate e

--------------------------------------------------------------------------------

testEvalTemplate :: Text -> IO ()
testEvalTemplate src =
  compileTemplate src testResolveInterp primCheckEnv >>= \case
    Left err -> putStrLn $ displayError src err
    Right code -> do
      runEval 10000 testGetInterp (evalTemplate primTermEnv code) >>= \case
        Left err -> putStrLn err
        Right t -> putStrLn t
