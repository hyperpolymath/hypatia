{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
-- SPDX-License-Identifier: PLMP-1.0-or-later
-- | Tests for Ruleset module

module RulesetSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T

import CicdHyperA.Ruleset

spec :: Spec
spec = do
  describe "Effect types" $ do
    it "Preventive rules prevent issues" $ do
      show (Preventive :: Effect) `shouldBe` "Preventive"

    it "Curative rules fix issues" $ do
      show (Curative :: Effect) `shouldBe` "Curative"

    it "Diagnostic rules report issues" $ do
      show (Diagnostic :: Effect) `shouldBe` "Diagnostic"

  describe "Pre-built rules" $ do
    it "requireDependabot has correct name" $ do
      getRuleName requireDependabot `shouldBe` "require-dependabot"

    it "requireSecurityMd has correct name" $ do
      getRuleName requireSecurityMd `shouldBe` "require-security-md"

    it "blockTypeScript has correct name" $ do
      getRuleName blockTypeScript `shouldBe` "block-typescript"

    it "blockGolang has correct name" $ do
      getRuleName blockGolang `shouldBe` "block-golang"

    it "blockPython has correct name" $ do
      getRuleName blockPython `shouldBe` "block-python"

    it "pinGitHubActions has correct name" $ do
      getRuleName pinGitHubActions `shouldBe` "pin-github-actions"

    it "requireWorkflowPermissions has correct name" $ do
      getRuleName requireWorkflowPermissions `shouldBe` "require-workflow-permissions"

    it "requireSpdxHeader has correct name" $ do
      getRuleName requireSpdxHeader `shouldBe` "require-spdx-header"

    it "requireCodeOwners has correct name" $ do
      getRuleName requireCodeOwners `shouldBe` "require-codeowners"

    it "requireBranchProtection has correct name" $ do
      getRuleName requireBranchProtection `shouldBe` "require-branch-protection"

  describe "Severity levels" $ do
    it "Critical > High > Medium > Low > Info" $ do
      Critical > High `shouldBe` True
      High > Medium `shouldBe` True
      Medium > Low `shouldBe` True
      Low > Info `shouldBe` True

    it "Severity is totally ordered" $ do
      compare Critical Info `shouldBe` GT
      compare Low High `shouldBe` LT
      compare Medium Medium `shouldBe` EQ

  describe "Language types" $ do
    it "TypeScript is a valid language" $ do
      show TypeScript `shouldBe` "TypeScript"

    it "Rust is a valid language" $ do
      show Rust `shouldBe` "Rust"

    it "ReScript is a valid language" $ do
      show ReScript `shouldBe` "ReScript"

  describe "Condition language" $ do
    it "FileExists creates a valid condition" $ do
      let cond = FileExists ".github/dependabot.yml"
      show cond `shouldContain` "FileExists"

    it "And combines conditions" $ do
      let cond = And (FileExists "a") (FileExists "b")
      show cond `shouldContain` "And"

    it "Or combines conditions" $ do
      let cond = Or (FileExists "a") (FileExists "b")
      show cond `shouldContain` "Or"

    it "Not negates conditions" $ do
      let cond = Not (FileExists "a")
      show cond `shouldContain` "Not"

  describe "Action types" $ do
    it "InjectFile creates a valid action" $ do
      let action = InjectFile "path" "content"
      show action `shouldContain` "InjectFile"

    it "RejectCommit creates a valid action" $ do
      let action = RejectCommit "reason"
      show action `shouldContain` "RejectCommit"

    it "Alert creates a valid action" $ do
      let action = Alert Medium "message"
      show action `shouldContain` "Alert"

  describe "Fix types" $ do
    it "ReplaceInFile creates a valid fix" $ do
      let fix = ReplaceInFile "path" "old" "new"
      show fix `shouldContain` "ReplaceInFile"

    it "CreatePR creates a valid fix" $ do
      let fix = CreatePR "title" "body"
      show fix `shouldContain` "CreatePR"

  describe "Ruleset container" $ do
    it "mkRuleset creates an empty ruleset" $ do
      let rs = mkRuleset "test" "description" "author" SecurityCategory
      getRuleCount rs `shouldBe` 0

    it "addRule adds a preventive rule" $ do
      let rs = mkRuleset "test" "description" "author" SecurityCategory
          rs' = addRule (SomeRule requireDependabot) rs
      getRuleCount rs' `shouldBe` 1

    it "addRule adds a curative rule" $ do
      let rs = mkRuleset "test" "description" "author" SecurityCategory
          rs' = addRule (SomeRule pinGitHubActions) rs
      getRuleCount rs' `shouldBe` 1

    it "addRule adds a diagnostic rule" $ do
      let rs = mkRuleset "test" "description" "author" SecurityCategory
          rs' = addRule (SomeRule requireBranchProtection) rs
      getRuleCount rs' `shouldBe` 1

    it "removeRule removes a rule by name" $ do
      let rs = mkRuleset "test" "description" "author" SecurityCategory
          rs' = addRule (SomeRule requireDependabot) rs
          rs'' = removeRule "require-dependabot" rs'
      getRuleCount rs'' `shouldBe` 0

    it "filterByEffect returns rules of correct type" $ do
      let rs = addRule (SomeRule requireDependabot) $
               addRule (SomeRule pinGitHubActions) $
               mkRuleset "test" "description" "author" SecurityCategory
      length (filterByEffect Preventive rs) `shouldBe` 1
      length (filterByEffect Curative rs) `shouldBe` 1

  describe "Pre-built rulesets" $ do
    it "rsrComplianceRuleset has multiple rules" $ do
      getRuleCount rsrComplianceRuleset `shouldSatisfy` (> 0)

    it "securityBaselineRuleset has multiple rules" $ do
      getRuleCount securityBaselineRuleset `shouldSatisfy` (> 0)

    it "openssfScorecard has multiple rules" $ do
      getRuleCount openssfScorecard `shouldSatisfy` (> 0)

    it "rsrComplianceRuleset is Compliance category" $ do
      metaCategory (rulesetMeta rsrComplianceRuleset) `shouldBe` ComplianceCategory

    it "securityBaselineRuleset is Security category" $ do
      metaCategory (rulesetMeta securityBaselineRuleset) `shouldBe` SecurityCategory

  describe "RulesetCategory" $ do
    it "SecurityCategory is valid" $ do
      show SecurityCategory `shouldBe` "SecurityCategory"

    it "ComplianceCategory is valid" $ do
      show ComplianceCategory `shouldBe` "ComplianceCategory"

    it "QualityCategory is valid" $ do
      show QualityCategory `shouldBe` "QualityCategory"

  describe "getRuleEffect" $ do
    it "returns Preventive for preventive rules" $ do
      getRuleEffect requireDependabot `shouldBe` Preventive

    it "returns Curative for curative rules" $ do
      getRuleEffect pinGitHubActions `shouldBe` Curative

    it "returns Diagnostic for diagnostic rules" $ do
      getRuleEffect requireBranchProtection `shouldBe` Diagnostic
