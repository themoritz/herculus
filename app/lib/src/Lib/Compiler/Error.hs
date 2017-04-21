{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Error where

import           Lib.Prelude

import           Data.List.NonEmpty        (NonEmpty ((:|)))
import           Data.Text                 as T (pack, unlines)

import qualified Text.Megaparsec           as P
import           Text.Megaparsec.Error     (parseErrorTextPretty)

import           Lib.Compiler.AST.Position

data Error = Error
  { errMsg  :: Text
  , errSpan :: Span
  } deriving (Show)

internalError :: MonadError Error m => Maybe Span -> Text -> m a
internalError mSpan msg =
  compileError (fromMaybe voidSpan mSpan) ("Internal error: " <> msg)

compileError :: MonadError Error m => Span -> Text -> m a
compileError span msg = throwError (Error (msg <> "\n") span)

convertParseError :: P.ParseError Char P.Dec -> Error
convertParseError err = Error msg span
  where msg = pack $ parseErrorTextPretty err
        pos :| _ = P.errorPos err
        span = Span pos pos

displayError :: Text -> Error -> Text
displayError src (Error msg span) = T.unlines $
  msg : highlightSpan False span src
