{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
-- SPDX-License-Identifier: PMPL-1.0-or-later
-- | Tests for Verify module

module VerifySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Hypatia.Ruleset
import Hypatia.Verify

spec :: Spec
spec = do
  describe "verifyRule" $ do
    it "accepts valid preventive rules" $ do
      verifyRule requireDependabot `shouldBe` Verified

    it "accepts valid curative rules" $ do
      verifyRule pinGitHubActions `shouldBe` Verified

    it "accepts valid diagnostic rules" $ do
      verifyRule requireBranchProtection `shouldBe` Verified

  describe "verifyRuleset" $ do
    it "verifies multiple valid rules" $ do
      let rules = [requireDependabot, requireSecurityMd]
      verifyRuleset rules `shouldBe` Verified

    it "verifies empty ruleset" $ do
      verifyRuleset ([] :: [Rule 'Preventive]) `shouldBe` Verified

  describe "verifyRulesetContainer" $ do
    it "verifies pre-built rulesets" $ do
      verifyRulesetContainer rsrComplianceRuleset `shouldBe` Verified
      verifyRulesetContainer securityBaselineRuleset `shouldBe` Verified
      verifyRulesetContainer openssfScorecard `shouldBe` Verified

    it "verifies empty container" $ do
      let rs = mkRuleset "test" "description" "author" SecurityCategory
      verifyRulesetContainer rs `shouldBe` Verified

  describe "checkNoConflicts" $ do
    it "finds no conflicts in pre-built rules" $ do
      let rules = [requireDependabot, requireSecurityMd, blockTypeScript]
      checkNoConflicts rules `shouldBe` Verified

    it "detects empty list as valid" $ do
      checkNoConflicts ([] :: [Rule 'Preventive]) `shouldBe` Verified

  describe "checkCoverage" $ do
    it "reports complete coverage" $ do
      let rules = [requireDependabot, requireSecurityMd]
          alerts = Set.fromList ["require-dependabot", "require-security-md"]
      checkCoverage rules alerts `shouldBe` Verified

    it "reports incomplete coverage" $ do
      let rules = [requireDependabot]
          alerts = Set.fromList ["require-dependabot", "missing-rule"]
      case checkCoverage rules alerts of
        Verified -> expectationFailure "Should have found incomplete coverage"
        VerifyFailed _ -> pure ()

  describe "checkCompleteness" $ do
    it "accepts complete curative rules" $ do
      checkCompleteness [pinGitHubActions] `shouldBe` Verified

  describe "QuickCheck properties" $ do
    describe "prop_ruleIdempotent" $ do
      it "holds for preventive rules" $ do
        prop_ruleIdempotent requireDependabot `shouldBe` True

      it "holds for curative rules" $ do
        prop_ruleIdempotent pinGitHubActions `shouldBe` True

      it "holds for diagnostic rules" $ do
        prop_ruleIdempotent requireBranchProtection `shouldBe` True

    describe "prop_fixResolves" $ do
      it "holds for pinGitHubActions" $ do
        prop_fixResolves pinGitHubActions `shouldBe` True

    describe "prop_noRuleConflicts" $ do
      it "holds for different rules" $ do
        prop_noRuleConflicts requireDependabot requireSecurityMd `shouldBe` True

      it "holds for same rule" $ do
        prop_noRuleConflicts requireDependabot requireDependabot `shouldBe` True

    describe "prop_deterministic" $ do
      it "holds for preventive rules" $ do
        prop_deterministic requireDependabot `shouldBe` True

      it "holds for curative rules" $ do
        prop_deterministic pinGitHubActions `shouldBe` True

    describe "prop_preventiveSafe" $ do
      it "holds for requireDependabot" $ do
        prop_preventiveSafe requireDependabot `shouldBe` True

      it "holds for blockTypeScript" $ do
        prop_preventiveSafe blockTypeScript `shouldBe` True

    describe "prop_curativeImproves" $ do
      it "holds for pinGitHubActions" $ do
        prop_curativeImproves pinGitHubActions `shouldBe` True

  describe "Arbitrary generators" $ do
    it "arbitraryCondition generates valid conditions" $ do
      let cond = arbitraryCondition 5
      show cond `shouldContain` "File"

    it "arbitraryAction generates valid actions" $ do
      let action = arbitraryAction 3
      show action `shouldContain` ""  -- Just check it doesn't crash

    it "arbitraryFix generates valid fixes" $ do
      let fix = arbitraryFix 4
      show fix `shouldContain` ""  -- Just check it doesn't crash

  describe "Verification combinators" $ do
    it "allVerified returns True for all Verified" $ do
      allVerified [Verified, Verified, Verified] `shouldBe` True

    it "allVerified returns False with any failure" $ do
      allVerified [Verified, VerifyFailed [], Verified] `shouldBe` False

    it "anyFailed returns True with any failure" $ do
      anyFailed [Verified, VerifyFailed [], Verified] `shouldBe` True

    it "anyFailed returns False for all Verified" $ do
      anyFailed [Verified, Verified, Verified] `shouldBe` False

    it "combineResults merges errors" $ do
      let err1 = InvalidPattern "r1" "error1"
          err2 = InvalidPattern "r2" "error2"
          result = combineResults [VerifyFailed [err1], VerifyFailed [err2]]
      case result of
        Verified -> expectationFailure "Should have combined errors"
        VerifyFailed errs -> length errs `shouldBe` 2

    it "combineResults returns Verified for all Verified" $ do
      combineResults [Verified, Verified] `shouldBe` Verified

  describe "VerifyError types" $ do
    it "ConflictingRules contains rule names" $ do
      let err = ConflictingRules "rule1" "rule2" "conflict"
      show err `shouldContain` "rule1"
      show err `shouldContain` "rule2"

    it "InvalidPattern contains rule name" $ do
      let err = InvalidPattern "my-rule" "bad pattern"
      show err `shouldContain` "my-rule"

    it "MissingDependency contains dependency name" $ do
      let err = MissingDependency "parent" "child"
      show err `shouldContain` "parent"

    it "DuplicateRuleName contains rule name" $ do
      let err = DuplicateRuleName "duplicate"
      show err `shouldContain` "duplicate"
