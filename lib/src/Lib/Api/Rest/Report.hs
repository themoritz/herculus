{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Lib.Api.Rest.Report where

import qualified Data.ByteString.Lazy         as BS
import qualified Data.Text                    as TS (Text)
import qualified Data.Text.Encoding           as TS (encodeUtf8)
import qualified Data.Text.Lazy               as TL (Text)
import qualified Data.Text.Lazy.Encoding      as TL (encodeUtf8)

import           Network.HTTP.Media.MediaType ((//), (/:))
import           Servant.API.ContentTypes     (Accept (..), MimeRender (..))

data PDF

instance Accept PDF where
  contentType _ = "application" // "pdf"

instance MimeRender PDF BS.ByteString where
  mimeRender _ = id

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML TL.Text where
  mimeRender _ = TL.encodeUtf8

instance MimeRender HTML TS.Text where
  mimeRender _ = BS.fromStrict . TS.encodeUtf8
