-- | natural transformation from HexlT IO to ExceptT ServantErr IO

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module HexlNat
  ( hexlToServant
  ) where

import           Lib.Prelude

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text.Encoding     as Text
import           Servant                ((:~>) (..), Handler, ServantErr (..),
                                         enter, err400, err401, err403, err500)
import           Servant.Utils.Enter    (Enter)

import           Monads                 (AppError (..), HexlEnv, HexlT,
                                         runHexlT)


hexlToServant :: (Enter h (HexlT IO :~> Handler) s)
             => HexlEnv -> h -> s
hexlToServant env = enter $ Nat $ \hexlAction ->
  liftIO (runHexlT env hexlAction)
    >>= either (throwError . appErrToServantErr) pure

appErrToServantErr :: AppError -> ServantErr
appErrToServantErr = \case
    ErrUser         msg -> err400 { errBody = toBS msg }
    ErrBug          msg -> err500 { errBody = toBS msg }
    ErrUnauthorized msg -> err401 { errBody = toBS msg }
    ErrForbidden    msg -> err403 { errBody = toBS msg }
  where toBS = LBS.fromStrict . Text.encodeUtf8
