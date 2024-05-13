-- | natural transformation from HexlT IO to ExceptT ServantErr IO

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HexlNat
  ( hexlToServant
  ) where

import           Lib.Prelude hiding (Handler)

import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text.Encoding     as Text
import           Servant                (Handler, err400, err401, err403, err500)
import           Servant.Server         (ServerError (errBody))

import           Monads                 (AppError (..), HexlEnv, HexlT,
                                         runHexlT)


hexlToServant
  :: HexlEnv -> HexlT IO a -> Handler a
hexlToServant env hexlAction =
  liftIO (runHexlT env hexlAction)
    >>= either (throwError . appErrToServantErr) pure

appErrToServantErr :: AppError -> ServerError
appErrToServantErr = \case
    ErrUser         msg -> err400 { errBody = toBS msg }
    ErrBug          msg -> err500 { errBody = toBS msg }
    ErrUnauthorized msg -> err401 { errBody = toBS msg }
    ErrForbidden    msg -> err403 { errBody = toBS msg }
  where toBS = LBS.fromStrict . Text.encodeUtf8
