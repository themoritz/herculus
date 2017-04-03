{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Env where

import           Lib.Prelude

import           Lib.Compiler.Type

tyRecord :: TypeF a
tyRecord = TypeConstructor "Record"
