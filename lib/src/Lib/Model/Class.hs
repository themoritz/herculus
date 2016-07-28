module Lib.Model.Class where

import Data.Bson
import Data.Text

class ToDocument a where
  toDocument :: a -> Document

class FromDocument a where
  parseDocument :: Document -> Either Text a
