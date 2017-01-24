{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Monad.Except     (ExceptT)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import           Data.Monoid              ((<>))
import qualified Data.Text.Encoding       as Text
import qualified Database.MongoDB         as Mongo
import           Diener                   (LogEnv, runDienerT)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import qualified Api
import           Handler                  (handlers)
import           Handler.Types            (HandlerT, runInHandlerEnv)
import           Options                  (Options (..), getOptions)

import           Types                    (AppError (..), Env (..))


app :: LogEnv Env -> Server Api.Routes
app env = enter transform handlers
  where
    transform :: HandlerT IO :~> ExceptT ServantErr IO
    transform = Nat $ \action ->
      liftIO (runDienerT env action) >>= either
        (throwError . appErrToServantErr)
        pure

    appErrToServantErr :: AppError -> ServantErr
    appErrToServantErr = \case
        ErrUser msg     -> err400 { errBody = toBS msg }
        ErrBug msg      -> err500 { errBody = toBS msg }
        ErrDatabase msg -> err500 { errBody = toBS msg }
      where
        toBS = ByteString.Lazy.fromStrict . Text.encodeUtf8

main :: IO ()
main = do
  Options{..} <- getOptions
  runInHandlerEnv optMongoHost optDbName $ \env -> do
    putStrLn $ "Listening on port " <> show optPort <> " ..."
    Warp.run optPort $ serve Api.api $ app env
