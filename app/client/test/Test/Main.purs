module Test.Main where

import Herculus.Prelude
import Herculus.Config.Column.Types (subTypes)
import Lib.Api.Schema.Compiler (Kind(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "subTypes" do
    it "works for higher kinded goal" do
      let
        goal = KindFun KindType KindType
        k = KindFun KindType (KindFun KindType KindType)
        res = subTypes goal k
      case res of
        Just [KindType] -> pure unit
        _ -> fail "Expected Just [KindType]"
