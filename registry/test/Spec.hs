-- SPDX-License-Identifier: PLMP-1.0-or-later
-- | Test suite entry point

module Main where

import Test.Hspec
import qualified RulesetSpec
import qualified VerifySpec
import qualified RegistrySpec
import qualified LiquidSpec
import qualified APISpec
import qualified CLISpec

main :: IO ()
main = hspec $ do
  describe "Ruleset" RulesetSpec.spec
  describe "Verify" VerifySpec.spec
  describe "Registry" RegistrySpec.spec
  describe "Liquid" LiquidSpec.spec
  describe "API" APISpec.spec
  describe "CLI" CLISpec.spec
