{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: PLMP-1.0-or-later
-- | Tests for Registry module

module RegistrySpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import CicdHyperA.Ruleset (Effect(..))
import CicdHyperA.Registry

spec :: Spec
spec = do
  describe "Registry initialization" $ do
    it "creates an empty registry" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        listRules reg `shouldBe` []
        regPath reg `shouldBe` tmpDir

    it "creates registry with default version" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        getVersion reg `shouldBe` RuleVersion 1 0 0

    it "saves and loads registry" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        _ <- registerRule "test-rule" "A test rule" "manual" reg

        mReg <- loadRegistry tmpDir
        case mReg of
          Nothing -> expectationFailure "Failed to load registry"
          Just loaded -> regPath loaded `shouldBe` tmpDir

  describe "Rule management" $ do
    it "registers a new rule" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        newReg <- registerRule "test-rule" "A test rule" "manual" reg
        length (listRules newReg) `shouldBe` 1

    it "looks up registered rule" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        newReg <- registerRule "test-rule" "A test rule" "manual" reg
        case lookupRule "test-rule" newReg of
          Nothing -> expectationFailure "Rule not found"
          Just entry -> entryName entry `shouldBe` "test-rule"

    it "unregisters a rule" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- registerRule "test-rule" "A test rule" "manual" reg
        reg'' <- unregisterRule "test-rule" reg'
        lookupRule "test-rule" reg'' `shouldBe` Nothing

    it "updates a rule" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- registerRule "test-rule" "Original" "manual" reg
        reg'' <- updateRule "test-rule" (\e -> e { entryDescription = "Updated" }) reg'
        case lookupRule "test-rule" reg'' of
          Nothing -> expectationFailure "Rule not found"
          Just entry -> entryDescription entry `shouldBe` "Updated"

  describe "Deposit/Withdraw operations" $ do
    it "deposits a new ruleset" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        newReg <- deposit "my-ruleset" "Description" "content" "manual" (Just Preventive) reg
        length (listRules newReg) `shouldBe` 1

    it "deposits with version bump" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "my-ruleset" "v1" "content1" "manual" Nothing reg
        reg'' <- deposit "my-ruleset" "v2" "content2" "manual" Nothing reg'
        case lookupRule "my-ruleset" reg'' of
          Nothing -> expectationFailure "Rule not found"
          Just entry -> entryVersion entry `shouldBe` RuleVersion 1 0 1

    it "withdraws a ruleset" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "my-ruleset" "Description" "content" "manual" Nothing reg
        result <- withdraw "my-ruleset" Nothing reg'
        case result of
          Nothing -> expectationFailure "Withdraw failed"
          Just (content, _) -> content `shouldBe` "content"

    it "withdraws specific version" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "my-ruleset" "Description" "content" "manual" Nothing reg
        result <- withdrawVersion "my-ruleset" (RuleVersion 1 0 0) reg'
        case result of
          Nothing -> expectationFailure "Withdraw failed"
          Just (content, _) -> content `shouldBe` "content"

    it "increments download count on withdraw" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "my-ruleset" "Description" "content" "manual" Nothing reg
        result <- withdraw "my-ruleset" Nothing reg'
        case result of
          Nothing -> expectationFailure "Withdraw failed"
          Just (_, entry) -> entryDownloads entry `shouldBe` 1

  describe "Search operations" $ do
    it "searches with empty query" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let query = SearchQuery Nothing Nothing Nothing Nothing [] Nothing Nothing 20 0 "name" False
        let result = search query reg
        resultTotal result `shouldBe` 0

    it "searches by text" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "security-rules" "Security ruleset" "content" "manual" Nothing reg
        reg'' <- deposit "compliance-rules" "Compliance ruleset" "content" "manual" Nothing reg'
        let query = SearchQuery (Just "security") Nothing Nothing Nothing [] Nothing Nothing 20 0 "name" False
        let result = search query reg''
        resultTotal result `shouldBe` 1

    it "respects limit parameter" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "rule1" "desc1" "c1" "manual" Nothing reg
        reg'' <- deposit "rule2" "desc2" "c2" "manual" Nothing reg'
        reg''' <- deposit "rule3" "desc3" "c3" "manual" Nothing reg''
        let query = SearchQuery Nothing Nothing Nothing Nothing [] Nothing Nothing 2 0 "name" False
        let result = search query reg'''
        length (resultEntries result) `shouldBe` 2
        resultHasMore result `shouldBe` True

    it "respects offset parameter" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "rule1" "desc1" "c1" "manual" Nothing reg
        reg'' <- deposit "rule2" "desc2" "c2" "manual" Nothing reg'
        let query = SearchQuery Nothing Nothing Nothing Nothing [] Nothing Nothing 20 1 "name" False
        let result = search query reg''
        length (resultEntries result) `shouldBe` 1

    it "searches by effect" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "prev-rule" "desc" "c" "manual" (Just Preventive) reg
        reg'' <- deposit "cure-rule" "desc" "c" "manual" (Just Curative) reg'
        length (searchByEffect Preventive reg'') `shouldBe` 1
        length (searchByEffect Curative reg'') `shouldBe` 1

  describe "Version management" $ do
    it "shows version correctly" $ do
      showVersion (RuleVersion 1 2 3) `shouldBe` "1.2.3"

    it "parses version correctly" $ do
      parseVersion "1.2.3" `shouldBe` Just (RuleVersion 1 2 3)

    it "rejects invalid version" $ do
      parseVersion "invalid" `shouldBe` Nothing
      parseVersion "1.2" `shouldBe` Nothing

    it "compares versions correctly" $ do
      compareVersions (RuleVersion 1 0 0) (RuleVersion 2 0 0) `shouldBe` LT
      compareVersions (RuleVersion 1 1 0) (RuleVersion 1 0 0) `shouldBe` GT
      compareVersions (RuleVersion 1 0 1) (RuleVersion 1 0 0) `shouldBe` GT
      compareVersions (RuleVersion 1 0 0) (RuleVersion 1 0 0) `shouldBe` EQ

    it "bumps version correctly" $ do
      bumpVersion "major" (RuleVersion 1 2 3) `shouldBe` RuleVersion 2 0 0
      bumpVersion "minor" (RuleVersion 1 2 3) `shouldBe` RuleVersion 1 3 0
      bumpVersion "patch" (RuleVersion 1 2 3) `shouldBe` RuleVersion 1 2 4

    it "sets version" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        newReg <- setVersion (RuleVersion 2 0 0) reg
        getVersion newReg `shouldBe` RuleVersion 2 0 0

  describe "Audit operations" $ do
    it "records audit entries on deposit" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        newReg <- deposit "test-rule" "desc" "content" "manual" Nothing reg
        let auditLog = regAudit newReg
        length auditLog `shouldBe` 1

    it "records audit entries on unregister" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "test-rule" "desc" "content" "manual" Nothing reg
        reg'' <- unregisterRule "test-rule" reg'
        let auditLog = regAudit reg''
        length auditLog `shouldBe` 2

    it "gets recent audit log" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        reg' <- deposit "r1" "d1" "c1" "manual" Nothing reg
        reg'' <- deposit "r2" "d2" "c2" "manual" Nothing reg'
        let recent = getAuditLog 1 reg''
        length recent `shouldBe` 1

  describe "Default configuration" $ do
    it "has sensible defaults" $ do
      configStoragePath defaultConfig `shouldBe` ".cicd-hyper-a"
      configVerifyOnDeposit defaultConfig `shouldBe` True
      configRequireSignature defaultConfig `shouldBe` False
      configMaxVersions defaultConfig `shouldBe` 10
      configAuditEnabled defaultConfig `shouldBe` True
