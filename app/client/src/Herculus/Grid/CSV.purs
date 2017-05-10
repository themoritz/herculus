module Herculus.Grid.CSV where

import Herculus.Prelude
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (fromFoldable, intercalate, many, zipWith)
import Data.JSDate (isValid, parse)
import Data.String (fromCharArray, singleton)
import Lib.Custom (ValNumber(..), ValTime(..), fromJSDate, pInteger, pValNumber)
import Lib.Model.Cell (Value(..))
import Lib.Model.Column (DataType(..))
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (sepBy, try)
import Text.Parsing.Parser.String (char, noneOf, string, whiteSpace)

-- Array of rows, which are arrays of elements
type CSV a = Array (Array a)

parseCSV :: Char -> Array (Maybe DataType) -> String -> CSV (Maybe Value)
parseCSV sep types input = case runParser input pCSV of
  Left _ -> []
  Right table -> map goRow table

  where

  goRow = zipWith goCell types
  goCell mDt cell = case mDt of
    Nothing -> Nothing
    Just dt -> case runParser cell (pValue dt) of
      Left e -> Nothing
      Right v -> Just v

  pCSV   = fromFoldable <$> sepBy pRow (char '\n')
  pRow   = fromFoldable <$> sepBy pField (char sep)
  pField = fromCharArray <$> many (noneOf [sep, '\n'])

  pValue :: DataType -> Parser String Value
  pValue = case _ of
    DataBool ->
          try (string "true"  *> pure (VBool true))
      <|> try (string "True"  *> pure (VBool true))
      <|> try (string "yes"   *> pure (VBool true))
      <|> try (string "Yes"   *> pure (VBool true))
      <|> try (string "false" *> pure (VBool false))
      <|> try (string "False" *> pure (VBool false))
      <|> try (string "no"    *> pure (VBool false))
      <|> try (string "No"    *> pure (VBool false))
    DataString ->
      VString <$> pField
    DataNumber ->
      VNumber <$> pValNumber
    DataInteger ->
      VInteger <$> pInteger
    DataTime -> do
      str <- fromCharArray <$> many (noneOf [sep, '\n', '[', ']', ',', ' '])
      let jsdate = unsafePerformEff (parse str)
      if isValid jsdate
        then pure $ VTime $ unsafePerformEff $ fromJSDate jsdate
        else fail "could not parse date"
    DataRowRef _ -> fail "not implemented"
    DataList dt -> do
      _ <- whiteSpace
      _ <- string "["
      xs <- sepBy (whiteSpace *> pValue dt <* whiteSpace) (string ",")
      _ <- string "]"
      _ <- whiteSpace
      pure $ VList $ fromFoldable xs
    DataMaybe dt -> fail "not implemented"

showCSV :: Char -> CSV (Maybe Value) -> String
showCSV sep table =
  intercalate "\n" $
  map (intercalate (singleton sep) <<< map (maybe "" showValue)) table

  where

  showValue :: Value -> String
  showValue = case _ of
    VBool b -> show b
    VString s -> s
    VNumber (ValNumber n) -> n
    VInteger i -> show i
    VTime (ValTime t) -> t
    VRowRef mr -> show mr
    VData l vs -> let go v = " (" <> showValue v <> ")" in
      l <> intercalate "" (map go vs)
    VRecord fields -> let go (Tuple f v) = f <> ": " <> showValue v in
      "{ " <> intercalate ", " (map go fields) <> " }"
    VList vals -> "[ " <> intercalate ", " (map showValue vals) <> " ]"
    VMaybe mV -> show $ map showValue mV
