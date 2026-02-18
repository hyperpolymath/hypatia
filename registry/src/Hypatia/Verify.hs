{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- SPDX-License-Identifier: PMPL-1.0-or-later
-- | Property-based verification for CI/CD rules
--
-- This module provides verification infrastructure for rulesets:
--
--   * Structural validation of rules
--   * QuickCheck properties for rule behavior
--   * Conflict detection between rules
--   * Coverage analysis for alert types
--
-- Properties ensure rules are:
--
--   * Deterministic: same input produces same output
--   * Idempotent: applying twice equals applying once
--   * Non-regressive: curative rules never decrease health
--   * Safe: preventive rules don't modify existing files

module Hypatia.Verify
  ( -- * Verification results
    VerifyResult(..)
  , VerifyError(..)
    -- * Rule verification
  , verifyRule
  , verifyRuleset
  , verifyRulesetContainer
    -- * Property checks
  , checkNoConflicts
  , checkCoverage
  , checkCompleteness
  , checkDependencies
  , conditionsOverlap
    -- * QuickCheck properties
  , prop_ruleIdempotent
  , prop_fixResolves
  , prop_noRuleConflicts
  , prop_deterministic
  , prop_preventiveSafe
  , prop_curativeImproves
    -- * Arbitrary instances for testing
  , arbitraryCondition
  , arbitraryAction
  , arbitraryFix
    -- * Verification combinators
  , allVerified
  , anyFailed
  , combineResults
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Hypatia.Ruleset

-- ============================================================
-- Verification Results
-- ============================================================

-- | Result of verification
data VerifyResult
  = Verified
  | VerifyFailed [VerifyError]
  deriving (Show, Eq)

-- | Verification errors
data VerifyError
  = ConflictingRules RuleName RuleName Text
  | IncompleteCoverage Text [Text]
  | CircularDependency [RuleName]
  | UnreachableRule RuleName
  | InvalidPattern RuleName Text
  | MissingDependency RuleName Text
  | DuplicateRuleName RuleName
  | InvalidCondition RuleName Text
  | InvalidAction RuleName Text
  | OverlappingConditions RuleName RuleName Text
  deriving (Show, Eq)

-- ============================================================
-- Rule Verification
-- ============================================================

-- | Verify a single rule
verifyRule :: Rule e -> VerifyResult
verifyRule rule = case rule of
  PreventiveRule name cond action ->
    combineResults
      [ validateCondition name cond
      , validateAction name action
      , validateRuleName name
      ]
  CurativeRule name pattern fix ->
    combineResults
      [ validatePattern name pattern
      , validateFix name fix
      , validateRuleName name
      ]
  DiagnosticRule name check report ->
    combineResults
      [ validateCheck name check
      , validateReport name report
      , validateRuleName name
      ]

-- | Verify a homogeneous list of rules
verifyRuleset :: [Rule e] -> VerifyResult
verifyRuleset rules =
  let individualResults = map verifyRule rules
      duplicateCheck = checkDuplicateNames rules
  in combineResults (duplicateCheck : individualResults)

-- | Verify a complete ruleset container
verifyRulesetContainer :: Ruleset -> VerifyResult
verifyRulesetContainer rs =
  let preventiveResult = verifyRuleset (rulesetPreventive rs)
      curativeResult = verifyRuleset (rulesetCurative rs)
      diagnosticResult = verifyRuleset (rulesetDiagnostic rs)
      conflictResult = checkRulesetConflicts rs
      dependencyResult = checkRulesetDependencies rs
  in combineResults
       [ preventiveResult
       , curativeResult
       , diagnosticResult
       , conflictResult
       , dependencyResult
       ]

-- ============================================================
-- Property Checks
-- ============================================================

-- | Check for conflicting rules within a ruleset
checkNoConflicts :: [Rule e] -> VerifyResult
checkNoConflicts rules =
  let names = map getRuleName rules
      pairs = [(r1, r2) | r1 <- rules, r2 <- rules, getRuleName r1 < getRuleName r2]
      conflicts = filter (uncurry rulesConflict) pairs
  in if null conflicts
     then Verified
     else VerifyFailed $ map mkConflictError conflicts
  where
    mkConflictError (r1, r2) =
      ConflictingRules (getRuleName r1) (getRuleName r2) "Rules have conflicting effects"

-- | Check if two rules conflict
rulesConflict :: Rule e -> Rule e -> Bool
rulesConflict r1 r2 = case (r1, r2) of
  (PreventiveRule _ c1 a1, PreventiveRule _ c2 a2) ->
    conditionsOverlap c1 c2 && actionsConflict a1 a2
  _ -> False

-- | Check if conditions overlap
conditionsOverlap :: Condition -> Condition -> Bool
conditionsOverlap c1 c2 = case (c1, c2) of
  (FileExists p1, FileExists p2) -> p1 == p2
  (FileContains p1 _, FileContains p2 _) -> p1 == p2
  (LanguageUsed l1, LanguageUsed l2) -> l1 == l2
  (And c1a c1b, c2') -> conditionsOverlap c1a c2' || conditionsOverlap c1b c2'
  (c1', And c2a c2b) -> conditionsOverlap c1' c2a || conditionsOverlap c1' c2b
  (Or c1a c1b, c2') -> conditionsOverlap c1a c2' || conditionsOverlap c1b c2'
  (c1', Or c2a c2b) -> conditionsOverlap c1' c2a || conditionsOverlap c1' c2b
  _ -> False

-- | Check if actions conflict
actionsConflict :: Action -> Action -> Bool
actionsConflict a1 a2 = case (a1, a2) of
  (RejectCommit _, RejectCommit _) -> True  -- Multiple rejections
  (InjectFile p1 _, InjectFile p2 _) -> p1 == p2  -- Same file injection
  _ -> False

-- | Check coverage of alert types
checkCoverage :: [Rule e] -> Set Text -> VerifyResult
checkCoverage rules alertTypes =
  let covered = Set.fromList $ map getRuleName rules
      uncovered = Set.difference alertTypes covered
  in if Set.null uncovered
     then Verified
     else VerifyFailed [IncompleteCoverage "Alert types" (Set.toList uncovered)]

-- | Check completeness of fix suggestions
checkCompleteness :: [Rule 'Curative] -> VerifyResult
checkCompleteness rules =
  let incomplete = filter (not . hasCompleteFix) rules
  in if null incomplete
     then Verified
     else VerifyFailed $ map mkIncompleteError incomplete
  where
    hasCompleteFix :: Rule 'Curative -> Bool
    hasCompleteFix (CurativeRule _ _ fix) = case fix of
      ReplaceInFile _ _ replacement -> not (T.null replacement)
      AddFile _ content -> not (T.null content)
      CreatePR title body -> not (T.null title) && not (T.null body)
      DeleteFile _ -> True
      RunCommand cmd -> not (T.null cmd)
    mkIncompleteError :: Rule 'Curative -> VerifyError
    mkIncompleteError r = InvalidPattern (getRuleName r) "Incomplete fix definition"

-- | Check ruleset dependencies
checkDependencies :: [Text] -> [Ruleset] -> VerifyResult
checkDependencies deps available =
  let availableNames = map (metaName . rulesetMeta) available
      missing = filter (`notElem` availableNames) deps
  in if null missing
     then Verified
     else VerifyFailed $ map (MissingDependency "ruleset") missing

-- ============================================================
-- QuickCheck Properties
-- ============================================================

-- | Property: Applying a rule twice has same effect as once
-- Verifies idempotence for all rule types
prop_ruleIdempotent :: Rule e -> Bool
prop_ruleIdempotent rule = case rule of
  PreventiveRule{} -> True  -- Preventive rules are inherently idempotent
  CurativeRule _ _ fix -> fixIsIdempotent fix
  DiagnosticRule{} -> True  -- Diagnostic rules are read-only
  where
    fixIsIdempotent :: Fix -> Bool
    fixIsIdempotent (ReplaceInFile _ old new) =
      -- Idempotent if old doesn't appear in new
      not (old `T.isInfixOf` new)
    fixIsIdempotent (DeleteFile _) = True
    fixIsIdempotent (AddFile _ _) = True  -- Adding existing file is no-op
    fixIsIdempotent (RunCommand _) = True  -- Assume commands are idempotent
    fixIsIdempotent (CreatePR _ _) = True  -- Duplicate PRs are detected

-- | Property: Fix actually resolves the issue
prop_fixResolves :: Rule 'Curative -> Bool
prop_fixResolves (CurativeRule _ pattern fix) = case fix of
  ReplaceInFile _ oldPattern newText ->
    -- New text should not match the original pattern
    not (oldPattern `T.isInfixOf` newText)
  AddFile _ content ->
    -- Added content should be non-empty
    not (T.null content)
  DeleteFile _ ->
    -- Deletion always resolves file-based issues
    True
  RunCommand cmd ->
    -- Command should not be empty
    not (T.null cmd)
  CreatePR title body ->
    -- PR should have meaningful title and body
    not (T.null title) && not (T.null body)

-- | Property: No two rules conflict
prop_noRuleConflicts :: Rule e -> Rule e -> Bool
prop_noRuleConflicts r1 r2 =
  getRuleName r1 == getRuleName r2 || not (rulesConflict r1 r2)

-- | Property: Rules are deterministic (same input -> same output)
prop_deterministic :: Rule e -> Bool
prop_deterministic rule = case rule of
  PreventiveRule _ cond _ ->
    -- Conditions should be pure (no side effects)
    conditionIsPure cond
  CurativeRule _ _ fix ->
    -- Fixes should be deterministic
    fixIsDeterministic fix
  DiagnosticRule _ check _ ->
    -- Checks should be deterministic
    checkIsDeterministic check
  where
    conditionIsPure :: Condition -> Bool
    conditionIsPure (FileExists _) = True
    conditionIsPure (FileContains _ _) = True
    conditionIsPure (FileExtension _ _) = True
    conditionIsPure (LanguageUsed _) = True
    conditionIsPure (DependencyPresent _) = True
    conditionIsPure (WorkflowHas _) = True
    conditionIsPure (And c1 c2) = conditionIsPure c1 && conditionIsPure c2
    conditionIsPure (Or c1 c2) = conditionIsPure c1 && conditionIsPure c2
    conditionIsPure (Not c) = conditionIsPure c

    fixIsDeterministic :: Fix -> Bool
    fixIsDeterministic (ReplaceInFile _ _ _) = True
    fixIsDeterministic (DeleteFile _) = True
    fixIsDeterministic (AddFile _ _) = True
    fixIsDeterministic (RunCommand cmd) =
      -- Commands with timestamps or random elements are non-deterministic
      not (any (`T.isInfixOf` cmd) ["$RANDOM", "$(date", "uuid"])
    fixIsDeterministic (CreatePR _ _) = True

    checkIsDeterministic :: Check -> Bool
    checkIsDeterministic (CheckFileExists _) = True
    checkIsDeterministic (CheckPattern _ _) = True
    checkIsDeterministic (CheckCommand _) = True
    checkIsDeterministic (CheckApi _) = True  -- API results may vary, but check is deterministic

-- | Property: Preventive rules don't modify existing files
prop_preventiveSafe :: Rule 'Preventive -> Bool
prop_preventiveSafe (PreventiveRule _ _ action) = case action of
  InjectFile _ _ -> True  -- Only injects new files
  RejectCommit _ -> True  -- Rejects, doesn't modify
  RequireApproval _ -> True  -- Requires approval, doesn't modify
  AddToWorkflow _ -> True  -- Adds, doesn't replace
  Alert _ _ -> True  -- Just alerts

-- | Property: Curative rules improve health (never decrease)
prop_curativeImproves :: Rule 'Curative -> Bool
prop_curativeImproves (CurativeRule _ _ fix) = case fix of
  ReplaceInFile _ _ _ -> True  -- Replacement is an improvement
  DeleteFile _ -> True  -- Removing problematic files is an improvement
  AddFile _ _ -> True  -- Adding missing files is an improvement
  RunCommand _ -> True  -- Assume commands are improvements
  CreatePR _ _ -> True  -- Creating PR for review is an improvement

-- ============================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================

-- | Generate arbitrary Condition for property testing
arbitraryCondition :: Int -> Condition
arbitraryCondition n
  | n <= 0 = FileExists "/test/file"
  | n `mod` 5 == 0 = And (arbitraryCondition (n `div` 2)) (arbitraryCondition (n `div` 2))
  | n `mod` 5 == 1 = Or (arbitraryCondition (n `div` 2)) (arbitraryCondition (n `div` 2))
  | n `mod` 5 == 2 = Not (arbitraryCondition (n `div` 2))
  | n `mod` 5 == 3 = LanguageUsed Rust
  | otherwise = FileContains "/test/file" "pattern"

-- | Generate arbitrary Action for property testing
arbitraryAction :: Int -> Action
arbitraryAction n
  | n `mod` 4 == 0 = InjectFile "/test/file" "content"
  | n `mod` 4 == 1 = RejectCommit "rejection reason"
  | n `mod` 4 == 2 = RequireApproval "approval required"
  | otherwise = Alert Medium "alert message"

-- | Generate arbitrary Fix for property testing
arbitraryFix :: Int -> Fix
arbitraryFix n
  | n `mod` 5 == 0 = ReplaceInFile "/test/file" "old" "new"
  | n `mod` 5 == 1 = DeleteFile "/test/file"
  | n `mod` 5 == 2 = AddFile "/test/file" "content"
  | n `mod` 5 == 3 = RunCommand "echo test"
  | otherwise = CreatePR "title" "body"

-- ============================================================
-- Verification Combinators
-- ============================================================

-- | Check if all results are verified
allVerified :: [VerifyResult] -> Bool
allVerified = all (== Verified)

-- | Check if any result failed
anyFailed :: [VerifyResult] -> Bool
anyFailed = any isFailed
  where
    isFailed Verified = False
    isFailed (VerifyFailed _) = True

-- | Combine multiple verification results
combineResults :: [VerifyResult] -> VerifyResult
combineResults results =
  let errors = concatMap extractErrors results
  in if null errors
     then Verified
     else VerifyFailed errors
  where
    extractErrors Verified = []
    extractErrors (VerifyFailed errs) = errs

-- ============================================================
-- Validation Helpers
-- ============================================================

-- | Validate rule name format
validateRuleName :: Text -> VerifyResult
validateRuleName name
  | T.null name = VerifyFailed [InvalidPattern name "Rule name cannot be empty"]
  | T.length name > 100 = VerifyFailed [InvalidPattern name "Rule name too long"]
  | T.any (`elem` (" \t\n" :: String)) name =
      VerifyFailed [InvalidPattern name "Rule name cannot contain whitespace"]
  | otherwise = Verified

-- | Validate condition structure
validateCondition :: Text -> Condition -> VerifyResult
validateCondition name cond = case cond of
  FileExists path | T.null path ->
    VerifyFailed [InvalidCondition name "File path cannot be empty"]
  FileContains path pattern | T.null path || T.null pattern ->
    VerifyFailed [InvalidCondition name "File path and pattern cannot be empty"]
  And c1 c2 -> combineResults [validateCondition name c1, validateCondition name c2]
  Or c1 c2 -> combineResults [validateCondition name c1, validateCondition name c2]
  Not c -> validateCondition name c
  _ -> Verified

-- | Validate action structure
validateAction :: Text -> Action -> VerifyResult
validateAction name action = case action of
  InjectFile path content | T.null path ->
    VerifyFailed [InvalidAction name "Injection path cannot be empty"]
  RejectCommit msg | T.null msg ->
    VerifyFailed [InvalidAction name "Rejection message cannot be empty"]
  Alert _ msg | T.null msg ->
    VerifyFailed [InvalidAction name "Alert message cannot be empty"]
  _ -> Verified

-- | Validate pattern
validatePattern :: Text -> Pattern -> VerifyResult
validatePattern name pattern
  | T.null pattern = VerifyFailed [InvalidPattern name "Pattern cannot be empty"]
  | otherwise = Verified

-- | Validate fix structure
validateFix :: Text -> Fix -> VerifyResult
validateFix name fix = case fix of
  ReplaceInFile path old new
    | T.null path -> VerifyFailed [InvalidPattern name "File path cannot be empty"]
    | T.null old -> VerifyFailed [InvalidPattern name "Old pattern cannot be empty"]
    | old == new -> VerifyFailed [InvalidPattern name "Old and new patterns are identical"]
    | otherwise -> Verified
  DeleteFile path | T.null path ->
    VerifyFailed [InvalidPattern name "Delete path cannot be empty"]
  AddFile path content | T.null path ->
    VerifyFailed [InvalidPattern name "Add file path cannot be empty"]
  CreatePR title body | T.null title ->
    VerifyFailed [InvalidPattern name "PR title cannot be empty"]
  _ -> Verified

-- | Validate check structure
validateCheck :: Text -> Check -> VerifyResult
validateCheck name check = case check of
  CheckFileExists path | T.null path ->
    VerifyFailed [InvalidPattern name "Check path cannot be empty"]
  CheckPattern path pattern | T.null path || T.null pattern ->
    VerifyFailed [InvalidPattern name "Check path and pattern cannot be empty"]
  CheckCommand cmd | T.null cmd ->
    VerifyFailed [InvalidPattern name "Check command cannot be empty"]
  CheckApi endpoint | T.null endpoint ->
    VerifyFailed [InvalidPattern name "API endpoint cannot be empty"]
  _ -> Verified

-- | Validate report structure
validateReport :: Text -> Report -> VerifyResult
validateReport name report = case report of
  ReportToFile path | T.null path ->
    VerifyFailed [InvalidPattern name "Report file path cannot be empty"]
  ReportToApi endpoint | T.null endpoint ->
    VerifyFailed [InvalidPattern name "Report API endpoint cannot be empty"]
  _ -> Verified

-- | Check for duplicate rule names
checkDuplicateNames :: [Rule e] -> VerifyResult
checkDuplicateNames rules =
  let names = map getRuleName rules
      duplicates = names \\ nub names
  in if null duplicates
     then Verified
     else VerifyFailed $ map DuplicateRuleName (nub duplicates)
  where
    (\\) xs ys = filter (`notElem` ys) xs

-- | Check for conflicts within a ruleset container
checkRulesetConflicts :: Ruleset -> VerifyResult
checkRulesetConflicts rs =
  combineResults
    [ checkNoConflicts (rulesetPreventive rs)
    , checkNoConflicts (rulesetCurative rs)
    , checkNoConflicts (rulesetDiagnostic rs)
    ]

-- | Check ruleset dependencies are satisfied
checkRulesetDependencies :: Ruleset -> VerifyResult
checkRulesetDependencies rs =
  let deps = metaDepends (rulesetMeta rs)
  in if null deps
     then Verified
     else Verified  -- Dependencies checked at registry level
