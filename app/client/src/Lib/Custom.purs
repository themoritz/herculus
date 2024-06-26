module Lib.Custom where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (cons, some)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic (class Generic, gCompare, gEq)
import Data.JSDate (JSDate, LOCALE, parse)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (fromCharArray, singleton, toCharArray)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (char, oneOf)

newtype Id a = Id String
derive instance genericId :: Generic (Id a)
instance showId :: Show (Id a) where show (Id i) = i
instance eqId :: Eq (Id a) where eq = gEq
instance ordId :: Ord (Id a) where compare = gCompare

newtype Ref a = Ref { unRef :: String }
derive instance genericRef :: Generic (Ref a)

-- Id tags

data ColumnTag
data ProjectTag
data UserTag

newtype ValNumber = ValNumber String
derive instance genericValNumber :: Generic ValNumber

parseInteger :: String -> Maybe Int
parseInteger str = case runParser str pInteger of
  Left _ -> Nothing
  Right a -> Just a

pInteger :: Parser String Int
pInteger = do
  mPref <- optionMaybe $ oneOf ['+', '-']
  cs <- some $ oneOf $ toCharArray "0123456789"
  let abs = foldl (\x y -> x * 10 + y) 0 (map charToInt cs)
  pure case mPref of
    Just '-' -> -abs
    _ -> abs
  where
    charToInt = case _ of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9
      _   -> 0

parseValNumber :: String -> Maybe ValNumber
parseValNumber str = case runParser str pValNumber of
  Left _ -> Nothing
  Right a -> Just a

pValNumber :: Parser String ValNumber
pValNumber = do
  mPref <- optionMaybe $ oneOf ['+', '-']
  b <- some $ oneOf $ toCharArray "0123456789"
  c <- optionMaybe $ do
    d <- char '.'
    e <- some $ oneOf $ toCharArray "0123456789"
    pure (cons d e)
  pure $ ValNumber $
         maybe "" singleton mPref <>
         fromCharArray b <>
         maybe "" fromCharArray c

newtype ValTime = ValTime String
derive instance genericValTime :: Generic ValTime

foreign import fromJSDateImpl
  :: forall eff. JSDate -> Eff (locale :: LOCALE | eff) String

fromJSDate :: forall eff. JSDate -> Eff (locale :: LOCALE | eff) ValTime
fromJSDate jsdate = ValTime <$> fromJSDateImpl jsdate

toJSDate :: forall eff. ValTime -> Eff (locale :: LOCALE | eff) JSDate
toJSDate (ValTime str) = parse str
