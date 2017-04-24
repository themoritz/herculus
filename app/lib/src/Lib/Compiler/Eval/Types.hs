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
loadValue = \case
  VBool b    -> RData (if b then "True" else "False") []
  VString s  -> RString s
  VNumber n  -> RNumber n
  VInteger i -> RInteger i
  VTime t    -> RTime t
  VRowRef mr -> RRowRef mr
  VData l vs -> RData l (map loadValue vs)
  VRecord m  -> RRecord (map loadValue m)
  VList vs   -> go vs
    where go = \case
            []   -> RData "Nil" []
            a:as -> RData "Cons" [loadValue a, go as]
  VMaybe mv -> case mv of
    Nothing -> RData "Nothing" []
    Just v  -> RData "Just" [loadValue v]

storeValue :: Result -> Value
storeValue r = case r of
  RString s -> VString s
  RNumber n -> VNumber n
  RInteger i -> VInteger i
  RTime t -> VTime t
  RRowRef mr -> VRowRef mr
  RData l vs -> fromMaybe (VData l (map storeValue vs)) $
        tryList
    <|> tryMaybe
    <|> tryBoolean
    where
    tryList = if l `elem` ["Cons", "Nil"]
      then Just $ VList $ goList r
      else Nothing
      where goList = \case
              RData "Cons" [a, as] -> storeValue a : goList as
              RData "Nil" [] -> []
    tryMaybe = case (l, vs) of
      ("Nothing", []) -> Just $ VMaybe Nothing
      ("Just", [v])   -> Just $ VMaybe $ Just $ storeValue v
      _               -> Nothing
    tryBoolean = case (l, vs) of
      ("True", [])  -> Just $ VBool True
      ("False", []) -> Just $ VBool False
      _             -> Nothing
  RRecord m -> VRecord (map storeValue m)

data Result
  = RString Text
  | RNumber Number
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
  RString s -> textStrict s
  RNumber (Number n) -> double n
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
