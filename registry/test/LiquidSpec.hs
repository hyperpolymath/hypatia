{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: PLMP-1.0-or-later
-- | Tests for Liquid module (refinement types)

module LiquidSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import CicdHyperA.Liquid
import CicdHyperA.Ruleset

spec :: Spec
spec = do
  describe "Refined types" $ do
    it "clamps health score to [0, 100]" $ do
      let repo = mkRepo "test" 150 10 5 []
      repoHealthScore repo `shouldBe` 100

    it "clamps negative health to 0" $ do
      let repo = mkRepo "test" (-50) 10 5 []
      repoHealthScore repo `shouldBe` 0

    it "ensures non-negative file count" $ do
      let repo = mkRepo "test" 50 (-10) 5 []
      repoFileCount repo `shouldBe` 0

  describe "Curative rule application" $ do
    it "never decreases health score" $ property $ \health ->
      let clampedHealth = max 0 (min 100 (health :: Int))
          repo = mkRepo "test" clampedHealth 10 5 []
          result = verifiedApplyCurative pinGitHubActions repo
      in repoHealthScore result >= repoHealthScore repo

    it "decreases alert count" $ do
      let repo = mkRepo "test" 50 10 5 []
          result = verifiedApplyCurative pinGitHubActions repo
      repoAlertCount result `shouldBe` 4

  describe "Preventive rule application" $ do
    it "preserves existing files" $ do
      let existing = ["README.md", "Cargo.toml"]
          repo = mkRepo "test" 50 2 0 existing
          result = verifiedApplyPreventive requireDependabot repo
      repoExistingFiles result `shouldBe` existing

    it "may increase file count" $ do
      let repo = mkRepo "test" 50 2 0 []
          result = verifiedApplyPreventive requireDependabot repo
      repoFileCount result `shouldSatisfy` (>= repoFileCount repo)

  describe "Health improvement verification" $ do
    it "detects improvement" $ do
      let repo1 = mkRepo "test" 50 10 5 []
          repo2 = mkRepo "test" 60 10 4 []
      verifiedHealthImprovement repo1 repo2 `shouldBe` True

    it "detects no improvement" $ do
      let repo1 = mkRepo "test" 60 10 5 []
          repo2 = mkRepo "test" 50 10 6 []
      verifiedHealthImprovement repo1 repo2 `shouldBe` False

  describe "Proof combinators" $ do
    it "proves idempotence for curative rules" $ do
      let repo = mkRepo "test" 50 10 5 []
          f = verifiedApplyCurative pinGitHubActions
      proveIdempotent f repo `shouldBe` True

    it "proves no regression for curative rules" $ do
      let repo = mkRepo "test" 50 10 5 []
          f = verifiedApplyCurative pinGitHubActions
      proveNoRegression f repo `shouldBe` True

    it "proves determinism" $ do
      let repo = mkRepo "test" 50 10 5 []
          f = verifiedApplyCurative pinGitHubActions
      proveDeterministic f repo `shouldBe` True

  describe "Health score calculation" $ do
    it "starts at 100 with no alerts" $ do
      calculateHealthScore 0 True True True `shouldBe` 100

    it "penalizes alerts" $ do
      calculateHealthScore 10 True True True `shouldBe` 65  -- 100 - 50 + 15

    it "caps penalty at 100" $ do
      calculateHealthScore 100 False False False `shouldSatisfy` (>= 0)

  describe "Confidence calculation" $ do
    it "increases with occurrences" $ do
      let c1 = calculateConfidence 1 0.9 True
          c10 = calculateConfidence 10 0.9 True
      c10 > c1 `shouldBe` True

    it "decreases without severity match" $ do
      let withMatch = calculateConfidence 5 0.9 True
          withoutMatch = calculateConfidence 5 0.9 False
      withMatch > withoutMatch `shouldBe` True

    it "stays in [0, 1] range" $ property $ \(occ, rate) ->
      let occPos = max 0 (occ :: Int)
          rateClamp = max 0.0 (min 1.0 (rate :: Double))
          c = calculateConfidence occPos rateClamp True
      in c >= 0.0 && c <= 1.0
