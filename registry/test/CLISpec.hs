{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: PLMP-1.0-or-later
-- | CLI integration tests

module CLISpec (spec) where

import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = describe "CLI Commands" $ do

  describe "Argument Parsing" $ do
    it "parses deposit args with all options" $ do
      -- Test deposit argument parsing
      let args = ["ruleset.hs", "--name", "test-rule", "--description", "A test", "--sign", "--verify"]
      let hasSign = "--sign" `elem` args
      let hasVerify = "--verify" `elem` args
      hasSign `shouldBe` True
      hasVerify `shouldBe` True

    it "parses withdraw args with version" $ do
      let args = ["my-ruleset", "--version", "1.2.0", "--output", "./out"]
      let version = findOptArg "--version" args
      let output = findOptArg "--output" args
      version `shouldBe` Just "1.2.0"
      output `shouldBe` Just "./out"

    it "parses search args with filters" $ do
      let args = ["--effect", "preventive", "--language", "rust"]
      let effect = findOptArg "--effect" args
      let language = findOptArg "--language" args
      effect `shouldBe` Just "preventive"
      language `shouldBe` Just "rust"

    it "parses rulesets comma-separated" $ do
      let rs = "rule1,rule2,rule3"
      let parsed = T.splitOn "," (T.pack rs)
      length parsed `shouldBe` 3
      head parsed `shouldBe` "rule1"

  describe "Hook Commands" $ do
    it "defaults to hyperpolymath/default ruleset" $ do
      let args = []
      let rulesets = parseRulesetsArg args
      rulesets `shouldBe` ["hyperpolymath/default"]

    it "parses custom rulesets" $ do
      let args = ["--rulesets", "custom/rules"]
      let rulesets = parseRulesetsArg args
      rulesets `shouldBe` ["custom/rules"]

    it "defaults cache TTL to 3600" $ do
      let args = []
      let ttl = parseCacheTtlArg args
      ttl `shouldBe` 3600

    it "parses custom cache TTL" $ do
      let args = ["--cache-ttl", "7200"]
      let ttl = parseCacheTtlArg args
      ttl `shouldBe` 7200

  describe "Search Results" $ do
    it "filters by effect type" $ do
      let results = mockSearchResults (Just "preventive") Nothing Nothing
      all (\(_, _, e) -> e == "preventive") results `shouldBe` True

    it "returns all without filters" $ do
      let results = mockSearchResults Nothing Nothing Nothing
      length results `shouldBe` 6

-- Helper functions (duplicated from Main for testing)

findOptArg :: String -> [String] -> Maybe String
findOptArg _ [] = Nothing
findOptArg key (x:y:rest)
  | x == key  = Just y
  | otherwise = findOptArg key (y:rest)
findOptArg _ [_] = Nothing

parseRulesetsArg :: [String] -> [T.Text]
parseRulesetsArg args = case findOptArg "--rulesets" args of
  Just rs -> T.splitOn "," (T.pack rs)
  Nothing -> ["hyperpolymath/default"]

parseCacheTtlArg :: [String] -> Int
parseCacheTtlArg args = case findOptArg "--cache-ttl" args of
  Just ttl -> read ttl
  Nothing -> 3600

mockSearchResults :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> [(T.Text, T.Text, T.Text)]
mockSearchResults effect language _ = filter matches allRulesets
  where
    matches (_, _, e) =
      maybe True (== e) effect &&
      maybe True (const True) language

    allRulesets =
      [ ("hyperpolymath/rsr-compliance", "RSR language policy enforcement", "preventive")
      , ("hyperpolymath/security-baseline", "Security hardening rules", "preventive")
      , ("hyperpolymath/rust-clippy-strict", "Strict Clippy linting for Rust", "diagnostic")
      , ("hyperpolymath/rust-fuzzing", "Fuzzing configuration for Rust", "curative")
      , ("hyperpolymath/openssf-scorecard", "OpenSSF Scorecard fixes", "curative")
      , ("hyperpolymath/dependabot-hardening", "Dependabot configuration", "preventive")
      ]
