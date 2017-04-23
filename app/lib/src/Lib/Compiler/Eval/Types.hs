{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Eval.Types where

import           Lib.Prelude                  hiding (bool)

import qualified Data.Map                     as Map

import           Text.PrettyPrint.Leijen.Text

import           Lib.Model.Cell
import           Lib.Model.Row
import           Lib.Types

import           Lib.Compiler.Core
import           Lib.Compiler.Eval.Monad

type TermEnv = Map Text Result

termEnvDoc :: TermEnv -> Doc
termEnvDoc = vsep . map go . Map.toList
  where go (n, r) = textStrict n <> ":" <+> resultDoc r

termEnvPretty :: TermEnv -> Text
termEnvPretty = show . termEnvDoc

loadModule :: Map Text Expr -> TermEnv
loadModule = map (flip RContinuation Map.empty)

loadValue :: Value -> Result
loadValue = undefined

storeValue :: Result -> Value
storeValue = undefined

-- TODO: extend with eval value
data Result
  = RBoolean Bool
  | RString Text
  | RNumber Double
  | RInteger Integer
  | RTime Time
  | RRowRef (Maybe (Id Row))
  | RData Text [Result]
  | RRecord (Map Text Result)
  --
  | RClosure Binder Expr TermEnv
  | RContinuation Expr TermEnv
  | RPrimFun (Result -> Eval Result)

resultDoc :: Result -> Doc
resultDoc = \case
  RBoolean b -> bool b
  RString s -> textStrict s
  RNumber n -> double n
  RInteger i -> integer i
  RTime t -> textStrict $ show t
  RRowRef mr -> textStrict $ show mr
  RData n rs -> textStrict n <+> (hsep (map (parens . resultDoc) rs))
  RRecord m ->
    braces $ hsep $
    punctuate comma $
    map (\(f, r) -> textStrict f <> ":" <+> resultDoc r) $
    Map.toList m
  RClosure _ _ _ -> textStrict "closure"
  RContinuation _ _ -> textStrict "continuation"
  RPrimFun _ -> textStrict "prim"

prettyResult :: Result -> Text
prettyResult = show . resultDoc
