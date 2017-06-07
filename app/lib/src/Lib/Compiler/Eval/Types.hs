-- |

module Lib.Compiler.Eval.Types where

import           Lib.Prelude                  hiding (bool)

import qualified Data.Map                     as Map

import           Text.PrettyPrint.Leijen.Text hiding ((<$>))

import           Lib.Model.Cell
import           Lib.Model.Row
import           Lib.Types

import           Lib.Compiler.Core
import           Lib.Compiler.Eval.Monad

type TermEnv m = Map Text (Result m)

termEnvDoc :: TermEnv m -> Doc
termEnvDoc = vsep . map go . Map.toList
  where go (n, r) = textStrict n <> ":" <+> resultDoc r

termEnvPretty :: TermEnv m -> Text
termEnvPretty = show . termEnvDoc

loadModule :: Map Text Expr -> TermEnv m
loadModule = map (flip RContinuation Map.empty)

loadValue :: Value -> Result m
loadValue = \case
  VUndefined -> RUndefined
  VBool b    -> RData (if b then "True" else "False") []
  VString s  -> RString s
  VNumber n  -> RNumber n
  VInteger i -> RInteger i
  VTime t    -> RDateTime t
  VRowRef mr -> RRowRef mr
  VData l vs -> RData l (map loadValue vs)
  VRecord m  -> RRecord (map loadValue $ Map.fromList m)
  VList vs   -> go vs
    where go = \case
            []   -> RData "Nil" []
            a:as -> RData "Cons" [loadValue a, go as]
  VMaybe mv -> case mv of
    Nothing -> RData "Nothing" []
    Just v  -> RData "Just" [loadValue v]

storeValue :: Monad m => Result m -> Eval m Value
storeValue r = case r of
  RUndefined -> pure VUndefined
  RString s  -> pure $ VString s
  RNumber n  -> pure $ VNumber n
  RInteger i -> pure $ VInteger i
  RDateTime t    -> pure $ VTime t
  RRowRef mr -> pure $ VRowRef mr
  RData l vs -> fromMaybe (VData l <$> traverse storeValue vs) $
        tryList
    <|> tryMaybe
    <|> tryBoolean
    where
    tryList = if l `elem` ["Cons", "Nil"]
      then Just $ map VList $ goList r
      else Nothing
      where goList = \case
              RData "Cons" [a, as] -> (:) <$> storeValue a <*> goList as
              RData "Nil" [] -> pure []
              _ -> internalError "Found inconsistent list data type."
    tryMaybe = case (l, vs) of
      ("Nothing", []) -> Just $ pure $ VMaybe Nothing
      ("Just", [v])   -> Just $ map (VMaybe . Just) $ storeValue v
      _               -> Nothing
    tryBoolean = case (l, vs) of
      ("True", [])  -> Just $ pure $ VBool True
      ("False", []) -> Just $ pure $ VBool False
      _             -> Nothing
  RRecord m -> VRecord . Map.toList <$> traverse storeValue m
  RClosure _ _ _    -> internalError "Unexpected closure"
  RContinuation _ _ -> internalError "Unexpected continuation"
  RPrimFun _        -> internalError "Unexpected prim function"

data Result m
  = RUndefined
  | RString Text
  | RNumber Number
  | RInteger Integer
  | RDateTime Time
  | RRowRef (Maybe (Id Row))
  | RData Text [Result m]
  | RRecord (Map Text (Result m))
  --
  | RClosure Binder Expr (TermEnv m)
  | RContinuation Expr (TermEnv m)
  | RPrimFun (Result m -> Eval m (Result m))

resultDoc :: Result m -> Doc
resultDoc = \case
  RUndefined -> textStrict "<undefined>"
  RString s -> textStrict s
  RNumber (Number n) -> double n
  RInteger i -> integer i
  RDateTime t -> textStrict $ show t
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

prettyResult :: Result m -> Text
prettyResult = show . resultDoc
