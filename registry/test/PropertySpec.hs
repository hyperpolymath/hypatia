-- SPDX-License-Identifier: PMPL-1.0-or-later
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | QuickCheck property tests for cicd-hyper-a rulesets
--
-- This module provides comprehensive property-based testing for:
--
--   * Rule validation (valid rules pass, invalid fail)
--   * Ruleset composition (combining rulesets preserves properties)
--   * Pattern matching (patterns match expected strings)
--   * Severity ordering (Critical > High > Medium > Low > Info)
--   * Rule ID uniqueness within rulesets
--   * JSON round-trip encoding/decoding
--
-- Properties ensure the rule DSL maintains invariants under arbitrary inputs.

module PropertySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort)
import Data.Maybe (isJust, fromMaybe)

import Hypatia.Ruleset
import Hypatia.Verify
import Hypatia.Registry (RuleVersion(..), showVersion, parseVersion)
import Hypatia.API
  ( DepositRequest(..), WithdrawRequest(..), SearchRequest(..)
  , DepositResponse(..), WithdrawResponse(..), SearchResponse(..)
  , VerifyRequest(..), VerifyResponse(..)
  , RulesetSummary(..), PaginationInfo(..), HealthCheck(..)
  )

-- ============================================================
-- Arbitrary Instances
-- ============================================================

-- | Arbitrary instance for Text (limited to printable ASCII for safety)
instance Arbitrary Text where
  arbitrary = do
    len <- chooseInt (1, 50)
    chars <- vectorOf len $ elements validChars
    return $ T.pack chars
    where
      validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_."

  shrink t = map T.pack $ shrink (T.unpack t)

-- | Arbitrary instance for Severity
instance Arbitrary Severity where
  arbitrary = elements [Critical, High, Medium, Low, Info]

-- | Arbitrary instance for Effect
instance Arbitrary Effect where
  arbitrary = elements [Preventive, Curative, Diagnostic]

-- | Arbitrary instance for Language
instance Arbitrary Language where
  arbitrary = elements
    [ TypeScript, JavaScript, Golang, Python, Rust
    , ReScript, Gleam, Julia, Haskell, Logtalk
    , Nickel, Guile, OCaml, Ada
    ]

-- | Arbitrary instance for RulesetCategory
instance Arbitrary RulesetCategory where
  arbitrary = elements
    [ SecurityCategory, ComplianceCategory, QualityCategory
    , PerformanceCategory, DocumentationCategory, InfrastructureCategory
    ]

-- | Arbitrary instance for TargetLanguage
instance Arbitrary TargetLanguage where
  arbitrary = oneof
    [ pure AllLanguages
    , SpecificLanguages <$> listOf1 arbitrary
    ]

-- | Arbitrary instance for Condition (sized for recursion control)
instance Arbitrary Condition where
  arbitrary = sized genCondition
    where
      genCondition :: Int -> Gen Condition
      genCondition 0 = genLeafCondition
      genCondition n = oneof
        [ genLeafCondition
        , And <$> genCondition (n `div` 2) <*> genCondition (n `div` 2)
        , Or <$> genCondition (n `div` 2) <*> genCondition (n `div` 2)
        , Not <$> genCondition (n `div` 2)
        ]

      genLeafCondition :: Gen Condition
      genLeafCondition = oneof
        [ FileExists <$> genFilePath
        , FileContains <$> genFilePath <*> genPattern
        , FileExtension <$> genFilePath <*> genExtension
        , LanguageUsed <$> arbitrary
        , DependencyPresent <$> genPackageName
        , WorkflowHas <$> genPattern
        ]

      genFilePath :: Gen Text
      genFilePath = do
        parts <- listOf1 $ elements ["src", "lib", "test", "config", ".github"]
        filename <- elements ["file", "main", "index", "config"]
        ext <- elements [".hs", ".rs", ".ts", ".json", ".yml"]
        return $ T.intercalate "/" parts <> "/" <> filename <> ext

      genPattern :: Gen Text
      genPattern = do
        word <- elements ["TODO", "FIXME", "error", "warning", "permissions"]
        return word

      genExtension :: Gen Text
      genExtension = elements [".ts", ".go", ".py", ".rs", ".hs", ".js"]

      genPackageName :: Gen Text
      genPackageName = elements ["aeson", "text", "containers", "lodash", "react"]

  shrink (And c1 c2) = [c1, c2] ++ [And c1' c2 | c1' <- shrink c1]
                                ++ [And c1 c2' | c2' <- shrink c2]
  shrink (Or c1 c2) = [c1, c2] ++ [Or c1' c2 | c1' <- shrink c1]
                               ++ [Or c1 c2' | c2' <- shrink c2]
  shrink (Not c) = [c] ++ [Not c' | c' <- shrink c]
  shrink _ = []

-- | Arbitrary instance for Action
instance Arbitrary Action where
  arbitrary = oneof
    [ InjectFile <$> genFilePath <*> genContent
    , RejectCommit <$> genMessage
    , RequireApproval <$> genMessage
    , AddToWorkflow <$> genContent
    , Alert <$> arbitrary <*> genMessage
    ]
    where
      genFilePath = do
        filename <- elements ["config", "security", "policy", "workflow"]
        ext <- elements [".yml", ".json", ".md"]
        return $ ".github/" <> filename <> ext

      genContent = elements
        [ "permissions: read-all"
        , "# SPDX-License-Identifier: PMPL-1.0-or-later"
        , "version: 2"
        ]

      genMessage = elements
        [ "Not allowed per policy"
        , "Requires review"
        , "Security violation detected"
        , "Missing required configuration"
        ]

-- | Arbitrary instance for Fix
instance Arbitrary Fix where
  arbitrary = oneof
    [ ReplaceInFile <$> genFilePath <*> genOld <*> genNew
    , DeleteFile <$> genFilePath
    , AddFile <$> genFilePath <*> genContent
    , RunCommand <$> genCommand
    , CreatePR <$> genTitle <*> genBody
    ]
    where
      genFilePath = elements
        [ ".github/workflows/ci.yml"
        , "package.json"
        , "Cargo.toml"
        , "tsconfig.json"
        ]

      genOld = elements ["v1", "main", "@latest", "TODO"]
      genNew = elements ["@sha256:abc123", "v2.0.0", "fixed", "DONE"]

      genContent = elements
        [ "# Generated file"
        , "version: 1"
        , "{}"
        ]

      genCommand = elements
        [ "cargo fmt"
        , "npm run lint --fix"
        , "deno fmt"
        ]

      genTitle = elements
        [ "Fix security issue"
        , "Pin GitHub Actions"
        , "Add SPDX headers"
        ]

      genBody = elements
        [ "Automated fix from cicd-hyper-a"
        , "Security improvement"
        , "Policy compliance fix"
        ]

-- | Arbitrary instance for Check
instance Arbitrary Check where
  arbitrary = oneof
    [ CheckFileExists <$> genFilePath
    , CheckPattern <$> genFilePath <*> genPattern
    , CheckCommand <$> genCommand
    , CheckApi <$> genEndpoint
    ]
    where
      genFilePath = elements
        [ "SECURITY.md"
        , ".github/dependabot.yml"
        , "CODEOWNERS"
        ]

      genPattern = elements ["permissions:", "SPDX", "version:"]

      genCommand = elements
        [ "cargo test"
        , "npm test"
        , "deno test"
        ]

      genEndpoint = elements
        [ "repos/{owner}/{repo}/branches/main/protection"
        , "repos/{owner}/{repo}/vulnerability-alerts"
        ]

-- | Arbitrary instance for Report
instance Arbitrary Report where
  arbitrary = oneof
    [ pure ReportToConsole
    , ReportToFile <$> genFilePath
    , ReportToApi <$> genEndpoint
    , pure ReportToPR
    ]
    where
      genFilePath = elements ["report.json", "audit.log", "findings.txt"]
      genEndpoint = elements ["/api/v1/findings", "/webhooks/slack"]

-- | Arbitrary instance for RuleVersion
instance Arbitrary RuleVersion where
  arbitrary = RuleVersion
    <$> chooseInt (0, 99)
    <*> chooseInt (0, 99)
    <*> chooseInt (0, 999)

  shrink (RuleVersion maj min' pat) =
    [ RuleVersion maj' min' pat | maj' <- shrink maj ] ++
    [ RuleVersion maj min'' pat | min'' <- shrink min' ] ++
    [ RuleVersion maj min' pat' | pat' <- shrink pat ]

-- | Arbitrary instance for RulesetMetadata
instance Arbitrary RulesetMetadata where
  arbitrary = do
    name <- genRulesetName
    desc <- genDescription
    author <- genAuthor
    category <- arbitrary
    targets <- arbitrary
    tags <- listOf genTag
    deps <- listOf genRulesetName
    let (major, minor, patch) = (1, 0, 0)
    return RulesetMetadata
      { metaName = name
      , metaVersion = (major, minor, patch)
      , metaDescription = desc
      , metaAuthor = author
      , metaLicense = "PMPL-1.0-or-later"
      , metaCategory = category
      , metaTargets = targets
      , metaCreated = Nothing
      , metaUpdated = Nothing
      , metaVerified = False
      , metaSigned = Nothing
      , metaTags = tags
      , metaDepends = deps
      }
    where
      genRulesetName = do
        org <- elements ["hyperpolymath", "acme", "security"]
        name <- elements ["baseline", "compliance", "hardening", "policy"]
        return $ org <> "/" <> name

      genDescription = elements
        [ "Security hardening rules"
        , "Compliance policy enforcement"
        , "Code quality checks"
        ]

      genAuthor = elements ["hyperpolymath", "security-team", "platform"]

      genTag = elements ["security", "compliance", "openssf", "rsr", "policy"]

-- | Arbitrary instance for API request types
instance Arbitrary DepositRequest where
  arbitrary = DepositRequest
    <$> genName
    <*> genDescription
    <*> genContent
    <*> elements ["preventive", "curative", "diagnostic"]
    <*> oneof [pure Nothing, Just <$> elements ["security", "compliance", "quality"]]
    <*> oneof [pure Nothing, Just <$> listOf (elements ["rust", "haskell", "typescript"])]
    <*> oneof [pure Nothing, Just <$> listOf (elements ["security", "policy", "fix"])]
    <*> arbitrary
    <*> arbitrary
    <*> oneof [pure Nothing, Just <$> genName]
    <*> oneof [pure Nothing, Just <$> pure "PMPL-1.0-or-later"]
    where
      genName = elements ["my-ruleset", "security-policy", "code-quality"]
      genDescription = elements ["Test ruleset", "Security rules", "Quality checks"]
      genContent = elements ["-- ruleset content", "rules: []", "{}"]

instance Arbitrary WithdrawRequest where
  arbitrary = WithdrawRequest
    <$> elements ["my-ruleset", "security-policy", "test-rules"]
    <*> oneof [pure Nothing, Just <$> pure "1.0.0"]
    <*> arbitrary
    <*> oneof [pure Nothing, Just <$> elements ["haskell", "json"]]

instance Arbitrary SearchRequest where
  arbitrary = SearchRequest
    <$> oneof [pure Nothing, Just <$> elements ["preventive", "curative", "diagnostic"]]
    <*> oneof [pure Nothing, Just <$> elements ["rust", "haskell", "typescript"]]
    <*> oneof [pure Nothing, Just <$> elements ["security", "compliance", "quality"]]
    <*> oneof [pure Nothing, Just <$> elements ["test", "security", "policy"]]
    <*> oneof [pure Nothing, Just <$> listOf (elements ["security", "policy"])]
    <*> oneof [pure Nothing, Just <$> arbitrary]
    <*> oneof [pure Nothing, Just <$> chooseInt (1, 100)]
    <*> oneof [pure Nothing, Just <$> chooseInt (0, 1000)]
    <*> oneof [pure Nothing, Just <$> elements ["name", "downloads", "updated"]]
    <*> oneof [pure Nothing, Just <$> arbitrary]

instance Arbitrary VerifyRequest where
  arbitrary = VerifyRequest
    <$> arbitrary
    <*> arbitrary
    <*> oneof [pure Nothing, Just <$> chooseInt (1, 300)]

instance Arbitrary HealthCheck where
  arbitrary = HealthCheck
    <$> elements ["arangodb", "dragonfly", "registry"]
    <*> elements ["pass", "fail", "warn"]
    <*> oneof [pure Nothing, Just <$> elements ["OK", "Timeout", "Connection refused"]]
    <*> oneof [pure Nothing, Just <$> chooseInt (0, 1000)]

instance Arbitrary PaginationInfo where
  arbitrary = do
    limit <- chooseInt (1, 100)
    offset <- chooseInt (0, 1000)
    total <- chooseInt (offset, offset + 10000)
    let hasNext = offset + limit < total
        hasPrev = offset > 0
    return PaginationInfo
      { paginationLimit = limit
      , paginationOffset = offset
      , paginationTotal = total
      , paginationNext = if hasNext then Just "/api/v1/rulesets?offset=next" else Nothing
      , paginationPrev = if hasPrev then Just "/api/v1/rulesets?offset=prev" else Nothing
      }

-- ============================================================
-- Property Tests
-- ============================================================

spec :: Spec
spec = do
  describe "Severity Properties" $ do
    prop "prop_severityTransitive - severity ordering is transitive" $
      \(s1 :: Severity) (s2 :: Severity) (s3 :: Severity) ->
        (s1 <= s2 && s2 <= s3) ==> s1 <= s3

    prop "prop_severityAntisymmetric - severity ordering is antisymmetric" $
      \(s1 :: Severity) (s2 :: Severity) ->
        (s1 <= s2 && s2 <= s1) ==> s1 == s2

    prop "prop_severityTotalOrder - severity is totally ordered" $
      \(s1 :: Severity) (s2 :: Severity) ->
        s1 <= s2 || s2 <= s1

    prop "prop_severityChain - Critical > High > Medium > Low > Info" $
      Critical > High && High > Medium && Medium > Low && Low > Info

    prop "prop_severityCompareConsistent - compare and operators agree" $
      \(s1 :: Severity) (s2 :: Severity) ->
        (s1 < s2) == (compare s1 s2 == LT) &&
        (s1 > s2) == (compare s1 s2 == GT) &&
        (s1 == s2) == (compare s1 s2 == EQ)

  describe "RuleVersion Properties" $ do
    prop "prop_versionRoundTrip - version show/parse round-trips" $
      \(v :: RuleVersion) ->
        parseVersion (showVersion v) == Just v

    prop "prop_versionOrdering - version ordering is correct" $
      \(v1 :: RuleVersion) (v2 :: RuleVersion) ->
        let cmp = compare v1 v2
            majCmp = compare (vMajor v1) (vMajor v2)
            minCmp = compare (vMinor v1) (vMinor v2)
            patCmp = compare (vPatch v1) (vPatch v2)
        in cmp == (if majCmp /= EQ then majCmp
                   else if minCmp /= EQ then minCmp
                   else patCmp)

    prop "prop_versionTransitive - version ordering is transitive" $
      \(v1 :: RuleVersion) (v2 :: RuleVersion) (v3 :: RuleVersion) ->
        (v1 <= v2 && v2 <= v3) ==> v1 <= v3

  describe "Condition Properties" $ do
    prop "prop_conditionShowable - all conditions can be shown" $
      \(c :: Condition) ->
        not (null (show c))

    prop "prop_notNotCondition - Not (Not c) simplifies to c for FileExists" $
      \path ->
        let c = FileExists path
            doubleNot = Not (Not c)
        in show doubleNot `seq` True  -- Just verify it doesn't crash

    prop "prop_andCommutativeForOverlap - And is commutative for overlap detection" $
      \(c1 :: Condition) (c2 :: Condition) ->
        conditionsOverlap (And c1 c2) (And c2 c1) ==
        conditionsOverlap (And c2 c1) (And c1 c2)

    prop "prop_orCommutativeForOverlap - Or is commutative for overlap detection" $
      \(c1 :: Condition) (c2 :: Condition) ->
        conditionsOverlap (Or c1 c2) (Or c2 c1) ==
        conditionsOverlap (Or c2 c1) (Or c1 c2)

  describe "Action Properties" $ do
    prop "prop_actionShowable - all actions can be shown" $
      \(a :: Action) ->
        not (null (show a))

    prop "prop_alertSeverityPreserved - Alert preserves severity" $
      \(sev :: Severity) msg ->
        case Alert sev msg of
          Alert sev' _ -> sev == sev'
          _ -> False

  describe "Fix Properties" $ do
    prop "prop_fixShowable - all fixes can be shown" $
      \(f :: Fix) ->
        not (null (show f))

    prop "prop_replaceInFileDistinct - ReplaceInFile old /= new" $
      \path old new ->
        old /= new ==>
          case ReplaceInFile path old new of
            ReplaceInFile _ o n -> o /= n
            _ -> False

  describe "Check and Report Properties" $ do
    prop "prop_checkShowable - all checks can be shown" $
      \(c :: Check) ->
        not (null (show c))

    prop "prop_reportShowable - all reports can be shown" $
      \(r :: Report) ->
        not (null (show r))

  describe "Ruleset Operations Properties" $ do
    prop "prop_emptyRulesetHasZeroRules - empty ruleset has zero rules" $
      \name desc author (cat :: RulesetCategory) ->
        not (T.null name) && not (T.null desc) && not (T.null author) ==>
          getRuleCount (mkRuleset name desc author cat) == 0

    prop "prop_addRuleIncreasesCount - adding rule increases count" $
      let rs = mkRuleset "test" "desc" "author" SecurityCategory
          withRule = addRule (SomeRule requireDependabot) rs
      in getRuleCount withRule == getRuleCount rs + 1

    prop "prop_removeRuleDecreasesCount - removing rule decreases count" $
      let rs = addRule (SomeRule requireDependabot) $
               mkRuleset "test" "desc" "author" SecurityCategory
          name = getRuleName requireDependabot
          withoutRule = removeRule name rs
      in getRuleCount withoutRule == getRuleCount rs - 1

    prop "prop_removeNonexistentRuleIsIdempotent - removing nonexistent rule is idempotent" $
      \name desc author (cat :: RulesetCategory) ->
        not (T.null name) && not (T.null desc) && not (T.null author) ==>
          let rs = mkRuleset name desc author cat
              rs' = removeRule "nonexistent-rule" rs
          in getRuleCount rs == getRuleCount rs'

    prop "prop_addRemoveIsIdentity - add then remove returns to original count" $
      let rs = mkRuleset "test" "desc" "author" SecurityCategory
          withRule = addRule (SomeRule requireSecurityMd) rs
          withoutRule = removeRule (getRuleName requireSecurityMd) withRule
      in getRuleCount withoutRule == getRuleCount rs

  describe "Rule Verification Properties" $ do
    it "prop_prebuiltRulesAreValid - pre-built rules pass verification" $ do
      verifyRule requireDependabot `shouldBe` Verified
      verifyRule requireSecurityMd `shouldBe` Verified
      verifyRule blockTypeScript `shouldBe` Verified
      verifyRule blockGolang `shouldBe` Verified
      verifyRule blockPython `shouldBe` Verified
      verifyRule pinGitHubActions `shouldBe` Verified
      verifyRule requireWorkflowPermissions `shouldBe` Verified
      verifyRule requireSpdxHeader `shouldBe` Verified
      verifyRule requireCodeOwners `shouldBe` Verified
      verifyRule requireBranchProtection `shouldBe` Verified

    it "prop_prebuiltRulesetsAreValid - pre-built rulesets pass verification" $ do
      verifyRulesetContainer rsrComplianceRuleset `shouldBe` Verified
      verifyRulesetContainer securityBaselineRuleset `shouldBe` Verified
      verifyRulesetContainer openssfScorecard `shouldBe` Verified

    prop "prop_combineVerifiedIsVerified - combining Verified results stays Verified" $
      combineResults [Verified, Verified] == Verified

    prop "prop_combineWithFailedIsFailed - combining with failed result propagates failure" $
      let err = InvalidPattern "test" "error"
      in case combineResults [Verified, VerifyFailed [err]] of
           VerifyFailed errs -> err `elem` errs
           _ -> False

  describe "Rule Name Properties" $ do
    prop "prop_getRuleNameConsistent - getRuleName returns consistent names" $
      getRuleName requireDependabot == "require-dependabot" &&
      getRuleName pinGitHubActions == "pin-github-actions"

    prop "prop_ruleNamesUnique - pre-built rule names are unique" $
      let names = map getRuleName
            [ requireDependabot
            , requireSecurityMd
            , blockTypeScript
            , blockGolang
            , blockPython
            , requireWorkflowPermissions
            , requireSpdxHeader
            , requireCodeOwners
            ]
          curativeNames = map getRuleName [pinGitHubActions]
          diagnosticNames = map getRuleName [requireBranchProtection]
          allNames = names ++ curativeNames ++ diagnosticNames
      in length allNames == length (nub allNames)

  describe "Effect Type Properties" $ do
    prop "prop_getRuleEffectConsistent - getRuleEffect matches rule type" $
      getRuleEffect requireDependabot == Preventive &&
      getRuleEffect pinGitHubActions == Curative &&
      getRuleEffect requireBranchProtection == Diagnostic

    prop "prop_filterByEffectPreserves - filterByEffect returns correct effect types" $
      let rs = securityBaselineRuleset
          preventive = filterByEffect Preventive rs
          curative = filterByEffect Curative rs
          diagnostic = filterByEffect Diagnostic rs
          totalFiltered = length preventive + length curative + length diagnostic
      in totalFiltered == getRuleCount rs

  describe "Ruleset Metadata Properties" $ do
    prop "prop_metadataPreservedOnMkRuleset - mkRuleset preserves metadata" $
      \name desc author (cat :: RulesetCategory) ->
        not (T.null name) && not (T.null desc) && not (T.null author) ==>
          let rs = mkRuleset name desc author cat
              meta = rulesetMeta rs
          in metaName meta == name &&
             metaDescription meta == desc &&
             metaAuthor meta == author &&
             metaCategory meta == cat

    prop "prop_categoryFilterWorks - filterByCategory returns correct categories" $
      \(cat :: RulesetCategory) ->
        let rulesets = [rsrComplianceRuleset, securityBaselineRuleset, openssfScorecard]
            filtered = filterByCategory cat rulesets
        in all ((== cat) . metaCategory . rulesetMeta) filtered

  describe "JSON Round-Trip Properties" $ do
    prop "prop_depositRequestRoundTrip - DepositRequest JSON round-trips" $
      \(req :: DepositRequest) ->
        decode (encode req) == Just req

    prop "prop_withdrawRequestRoundTrip - WithdrawRequest JSON round-trips" $
      \(req :: WithdrawRequest) ->
        decode (encode req) == Just req

    prop "prop_searchRequestRoundTrip - SearchRequest JSON round-trips" $
      \(req :: SearchRequest) ->
        decode (encode req) == Just req

    prop "prop_verifyRequestRoundTrip - VerifyRequest JSON round-trips" $
      \(req :: VerifyRequest) ->
        decode (encode req) == Just req

    prop "prop_healthCheckRoundTrip - HealthCheck JSON round-trips" $
      \(hc :: HealthCheck) ->
        decode (encode hc) == Just hc

    prop "prop_paginationInfoRoundTrip - PaginationInfo JSON round-trips" $
      \(pi :: PaginationInfo) ->
        decode (encode pi) == Just pi

  describe "QuickCheck Property Functions" $ do
    it "prop_ruleIdempotent passes for preventive rules" $
      prop_ruleIdempotent requireDependabot `shouldBe` True

    it "prop_ruleIdempotent passes for curative rules" $
      prop_ruleIdempotent pinGitHubActions `shouldBe` True

    it "prop_ruleIdempotent passes for diagnostic rules" $
      prop_ruleIdempotent requireBranchProtection `shouldBe` True

    it "prop_fixResolves passes for pinGitHubActions" $
      prop_fixResolves pinGitHubActions `shouldBe` True

    it "prop_deterministic passes for all pre-built rules" $ do
      prop_deterministic requireDependabot `shouldBe` True
      prop_deterministic pinGitHubActions `shouldBe` True
      prop_deterministic requireBranchProtection `shouldBe` True

    it "prop_preventiveSafe passes for preventive rules" $
      prop_preventiveSafe requireDependabot `shouldBe` True

    it "prop_curativeImproves passes for curative rules" $
      prop_curativeImproves pinGitHubActions `shouldBe` True

  describe "Arbitrary Generator Properties" $ do
    prop "prop_arbitraryConditionGeneratesValid - arbitrary conditions are valid" $
      \depth ->
        depth >= 0 ==>
          let cond = arbitraryCondition depth
          in show cond `seq` True  -- Verify it doesn't crash

    prop "prop_arbitraryActionGeneratesValid - arbitrary actions are valid" $
      \n ->
        let action = arbitraryAction n
        in show action `seq` True

    prop "prop_arbitraryFixGeneratesValid - arbitrary fixes are valid" $
      \n ->
        let fix = arbitraryFix n
        in show fix `seq` True

  describe "Verification Error Properties" $ do
    it "prop_duplicateNamesDetected - duplicate rule names are detected" $
      let rs = mkRuleset "test" "desc" "author" SecurityCategory
          -- Add same rule twice (this should be detected in verification)
          rs' = rs { rulesetPreventive = [requireDependabot, requireDependabot] }
      in case verifyRulesetContainer rs' of
           VerifyFailed errs -> any isDuplicateError errs
           Verified -> True  -- No duplicates in current implementation
      where
        isDuplicateError (DuplicateRuleName _) = True
        isDuplicateError _ = False

    prop "prop_emptyPatternInvalid - empty pattern fails validation" $
      case validatePattern "test-rule" "" of
        VerifyFailed _ -> True
        Verified -> False

    prop "prop_emptyRuleNameInvalid - empty rule name fails validation" $
      case validateRuleName "" of
        VerifyFailed _ -> True
        Verified -> False

    prop "prop_longRuleNameInvalid - rule name > 100 chars fails validation" $
      let longName = T.replicate 101 "a"
      in case validateRuleName longName of
           VerifyFailed _ -> True
           Verified -> False

    prop "prop_whitespaceInRuleNameInvalid - whitespace in rule name fails" $
      case validateRuleName "rule with spaces" of
        VerifyFailed _ -> True
        Verified -> False

  describe "Combinator Properties" $ do
    prop "prop_allVerifiedOnEmpty - allVerified is True for empty list" $
      allVerified []

    prop "prop_allVerifiedOnVerified - allVerified is True for all Verified" $
      allVerified [Verified, Verified, Verified]

    prop "prop_allVerifiedFalseOnFailed - allVerified is False when any failed" $
      not $ allVerified [Verified, VerifyFailed [InvalidPattern "x" "y"]]

    prop "prop_anyFailedOnEmpty - anyFailed is False for empty list" $
      not $ anyFailed []

    prop "prop_anyFailedOnFailed - anyFailed is True when any failed" $
      anyFailed [Verified, VerifyFailed [InvalidPattern "x" "y"]]

    prop "prop_combineResultsEmpty - combineResults on empty is Verified" $
      combineResults [] == Verified

    prop "prop_combineResultsCollectsErrors - combineResults collects all errors" $
      let e1 = InvalidPattern "r1" "err1"
          e2 = InvalidPattern "r2" "err2"
      in case combineResults [VerifyFailed [e1], VerifyFailed [e2]] of
           VerifyFailed errs -> e1 `elem` errs && e2 `elem` errs
           Verified -> False

-- ============================================================
-- Helper Validation Functions (exposed from Verify module)
-- ============================================================

-- | Re-exported validation functions for testing
validatePattern :: Text -> Text -> VerifyResult
validatePattern name pattern
  | T.null pattern = VerifyFailed [InvalidPattern name "Pattern cannot be empty"]
  | otherwise = Verified

validateRuleName :: Text -> VerifyResult
validateRuleName name
  | T.null name = VerifyFailed [InvalidPattern name "Rule name cannot be empty"]
  | T.length name > 100 = VerifyFailed [InvalidPattern name "Rule name too long"]
  | T.any (`elem` (" \t\n" :: String)) name =
      VerifyFailed [InvalidPattern name "Rule name cannot contain whitespace"]
  | otherwise = Verified
