{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Eval.Types where

import           Lib.Compiler.Core

import qualified Data.Map                     as Map
import           Text.PrettyPrint.Leijen.Text

import           Lib.Prelude


type TermEnv = Map Text Result

termEnvDoc :: TermEnv -> Doc
termEnvDoc = vsep . map go . Map.toList
  where go (n, r) = textStrict n <> ":" <+> resultDoc r

termEnvPretty :: TermEnv -> Text
termEnvPretty = show . termEnvDoc

type Eval = Except Text

loadModule :: Map Text Expr -> TermEnv
loadModule = map (flip RContinuation Map.empty)

data Value
  = VInt Integer
  | VNumber Double
  | VString Text
  | VData Text [Result]

valueDoc :: Value -> Doc
valueDoc = \case
  VInt i -> integer i
  VNumber n -> double n
  VString s -> textStrict s
  VData n rs -> textStrict n <+> (hsep (map (parens . resultDoc) rs))

data Result
  = RValue Value
  | RClosure Binder Expr TermEnv
  | RContinuation Expr TermEnv
  | RPrimFun (Result -> Eval Result)

resultDoc :: Result -> Doc
resultDoc = \case
  RValue v -> valueDoc v
  RClosure b e env -> textStrict "closure"
  RContinuation e env -> textStrict "continuation"
  RPrimFun _ -> textStrict "prim"

prettyResult :: Result -> Text
prettyResult = show . resultDoc
