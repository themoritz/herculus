{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Compiler.Check.Types where

import           Lib.Prelude

import           Control.Lens             (makeLenses)

import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.Map                 as Map

import qualified Lib.Compiler.Core        as Core
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Type

data Origin
  = Recursive
  | Default
  | Method
  deriving (Show)

data EnvType = EnvType
  { etPoly   :: PolyType
  , etOrigin :: Origin
  }

defaultEnvType :: PolyType -> EnvType
defaultEnvType = flip EnvType Default

data DictLookup
  -- | Class name, type variable
  = ByTypeVar Text Text
  -- | Class name, constructor name
  | ByConstructor Text Text
  -- | Type variable
  | AccessByTypeVar Text
  deriving (Eq, Ord, Show)

data CheckEnv = CheckEnv
  { _checkEnvKinds         :: Map Text Kind
  , _checkEnvTypes         :: Map Text EnvType
  , _checkEnvOperators     :: Map Text (Text, Fixity)
  , _checkEnvInstanceDicts :: Map DictLookup Text
  , _checkEnvClasses       :: Map Text Class
  }

mkCheckEnvOpTable :: CheckEnv -> OpTable
mkCheckEnvOpTable = Map.toList . map snd . _checkEnvOperators

unionCheckEnv :: CheckEnv -> CheckEnv -> CheckEnv
unionCheckEnv (CheckEnv k t o i c) (CheckEnv k' t' o' i' c') =
  CheckEnv (Map.union k k')
           (Map.union t t')
           (Map.union o o')
           (Map.union i i')
           (Map.union c c')

makeLenses ''CheckEnv

data CheckResult = CheckResult
  { resultCheckEnv :: CheckEnv
  , resultCore     :: HashMap Core.Ident Core.Expr
  , resultTycons   :: Map Text TyconInfo
  }

unionCheckResult :: CheckResult -> CheckResult -> CheckResult
unionCheckResult (CheckResult a b c) (CheckResult a' b' c') =
  CheckResult (a `unionCheckEnv` a') (b `HashMap.union` b') (c `Map.union` c')
