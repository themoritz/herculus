{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Env where

import           Lib.Prelude

import           Lib.Compiler.Type

tyRecord :: Type
tyRecord = mkTypeConstructor "Record"
