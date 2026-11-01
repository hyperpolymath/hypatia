{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Finite correspondence model for the Logtalk -> Elixir security-rule
-- migration (ECHIDNA rules/security_learning.lgt ->
-- Hypatia.Rules.SecurityErrors).
--
-- Scope, deliberately narrow (estate PROOF-POLICY: a checked model is not
-- a runtime-refinement proof):
--
--   * The source functions transcribe the 18 alert_category/2 facts, the
--     classify_severity/2 clauses (including the final low catch-all),
--     the six is_auto_fixable/1 facts, the domain of suggest_fix/2, and
--     get_prevention_workflow/2 from security_learning.lgt.
--   * The target functions transcribe the corresponding Elixir tables in
--     lib/rules/security_errors.ex over the same 18 alert types.
--   * The theorems check exhaustive agreement for category, severity,
--     auto-fixability, and fix presence. Prevention agrees on the six
--     shared, unchanged entries; the BranchProtectionID supersession and
--     the CodeReviewID extension are stated as separate lemmas rather
--     than hidden inside a false universal equality.
--   * Fix *text* is not modelled here: 10 of 13 strings are verbatim and
--     the other three are ledgered supersessions/generalisation in
--     docs/rules/logtalk-migration.adoc. Elixir runtime behaviour is
--     pinned by ExUnit tests, not by this file.
--
-- Intended Mathesis grade: Grade 3 target. It becomes Grade 3 only when
-- the strict command in verification/PROOF-STATUS.adoc has a recorded
-- successful receipt; until then its status is AGDA-PENDING.
-- No postulates, no unsafe options, no standard-library dependency.

module LogtalkMigration where

open import Agda.Builtin.Bool
open import Agda.Builtin.Equality

------------------------------------------------------------------------
-- Finite domains
------------------------------------------------------------------------

data Alert : Set where
  tokenPermissions         : Alert
  pinnedDependencies       : Alert
  missingWorkflowPerms     : Alert
  hardCodedCrypto          : Alert
  remotePropertyInjection  : Alert
  sqlInjection             : Alert
  unusedLocalVariable      : Alert
  syntaxError              : Alert
  vulnerabilities          : Alert
  unmaintained             : Alert
  securityPolicy           : Alert
  maintained               : Alert
  codeReview               : Alert
  branchProtection         : Alert
  ciiBestPractices         : Alert
  fuzzing                  : Alert
  sast                     : Alert
  ciTests                  : Alert

data Category : Set where
  workflowSecurity : Category
  codeSecurity     : Category
  codeQuality      : Category
  dependencyVuln   : Category
  processHygiene   : Category
  missingTests     : Category

data Severity : Set where
  critical : Severity
  high     : Severity
  medium   : Severity
  low      : Severity

data Workflow : Set where
  workflowLinter            : Workflow
  secretScanner             : Workflow
  cargoAudit                : Workflow
  scorecardEnforcer         : Workflow
  branchProtectionEnforcer  : Workflow

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A -> Maybe A

------------------------------------------------------------------------
-- Source: ECHIDNA security_learning.lgt
------------------------------------------------------------------------

logtalkCategory : Alert -> Category
logtalkCategory tokenPermissions        = workflowSecurity
logtalkCategory pinnedDependencies      = workflowSecurity
logtalkCategory missingWorkflowPerms    = workflowSecurity
logtalkCategory hardCodedCrypto         = codeSecurity
logtalkCategory remotePropertyInjection = codeSecurity
logtalkCategory sqlInjection            = codeSecurity
logtalkCategory unusedLocalVariable     = codeQuality
logtalkCategory syntaxError             = codeQuality
logtalkCategory vulnerabilities         = dependencyVuln
logtalkCategory unmaintained            = dependencyVuln
logtalkCategory securityPolicy          = processHygiene
logtalkCategory maintained              = processHygiene
logtalkCategory codeReview              = processHygiene
logtalkCategory branchProtection        = processHygiene
logtalkCategory ciiBestPractices        = processHygiene
logtalkCategory fuzzing                 = missingTests
logtalkCategory sast                    = missingTests
logtalkCategory ciTests                 = missingTests

-- The final Logtalk clause is classify_severity(_, low), so every alert
-- outside the critical/high/medium member lists is low here.
logtalkSeverity : Alert -> Severity
logtalkSeverity tokenPermissions        = high
logtalkSeverity pinnedDependencies      = medium
logtalkSeverity missingWorkflowPerms    = high
logtalkSeverity hardCodedCrypto         = critical
logtalkSeverity remotePropertyInjection = high
logtalkSeverity sqlInjection            = critical
logtalkSeverity unusedLocalVariable     = low
logtalkSeverity syntaxError             = medium
logtalkSeverity vulnerabilities         = critical
logtalkSeverity unmaintained            = low
logtalkSeverity securityPolicy          = medium
logtalkSeverity maintained              = low
logtalkSeverity codeReview              = medium
logtalkSeverity branchProtection        = medium
logtalkSeverity ciiBestPractices        = low
logtalkSeverity fuzzing                 = low
logtalkSeverity sast                    = low
logtalkSeverity ciTests                 = low

logtalkAutoFixable : Alert -> Bool
logtalkAutoFixable tokenPermissions        = true
logtalkAutoFixable pinnedDependencies      = true
logtalkAutoFixable missingWorkflowPerms    = true
logtalkAutoFixable hardCodedCrypto         = false
logtalkAutoFixable remotePropertyInjection = false
logtalkAutoFixable sqlInjection            = false
logtalkAutoFixable unusedLocalVariable     = true
logtalkAutoFixable syntaxError             = false
logtalkAutoFixable vulnerabilities         = false
logtalkAutoFixable unmaintained            = false
logtalkAutoFixable securityPolicy          = true
logtalkAutoFixable maintained              = false
logtalkAutoFixable codeReview              = false
logtalkAutoFixable branchProtection        = true
logtalkAutoFixable ciiBestPractices        = false
logtalkAutoFixable fuzzing                 = false
logtalkAutoFixable sast                    = false
logtalkAutoFixable ciTests                 = false

-- Domain of suggest_fix/2 only (13 facts); the strings themselves are
-- handled by the migration ledger and Elixir tests.
logtalkHasFix : Alert -> Bool
logtalkHasFix tokenPermissions        = true
logtalkHasFix pinnedDependencies      = true
logtalkHasFix missingWorkflowPerms    = true
logtalkHasFix hardCodedCrypto         = true
logtalkHasFix remotePropertyInjection = true
logtalkHasFix sqlInjection            = false
logtalkHasFix unusedLocalVariable     = true
logtalkHasFix syntaxError             = true
logtalkHasFix vulnerabilities         = true
logtalkHasFix unmaintained            = false
logtalkHasFix securityPolicy          = true
logtalkHasFix maintained              = false
logtalkHasFix codeReview              = true
logtalkHasFix branchProtection        = true
logtalkHasFix ciiBestPractices        = true
logtalkHasFix fuzzing                 = true
logtalkHasFix sast                    = false
logtalkHasFix ciTests                 = false

logtalkPrevention : Alert -> Maybe Workflow
logtalkPrevention tokenPermissions        = just workflowLinter
logtalkPrevention pinnedDependencies      = just workflowLinter
logtalkPrevention missingWorkflowPerms    = just workflowLinter
logtalkPrevention hardCodedCrypto         = just secretScanner
logtalkPrevention remotePropertyInjection = nothing
logtalkPrevention sqlInjection            = nothing
logtalkPrevention unusedLocalVariable     = nothing
logtalkPrevention syntaxError             = nothing
logtalkPrevention vulnerabilities         = just cargoAudit
logtalkPrevention unmaintained            = nothing
logtalkPrevention securityPolicy          = just scorecardEnforcer
logtalkPrevention maintained              = nothing
logtalkPrevention codeReview              = nothing
logtalkPrevention branchProtection        = just branchProtectionEnforcer
logtalkPrevention ciiBestPractices        = nothing
logtalkPrevention fuzzing                 = nothing
logtalkPrevention sast                    = nothing
logtalkPrevention ciTests                 = nothing

------------------------------------------------------------------------
-- Target: Hypatia.Rules.SecurityErrors (same 18 alert types)
------------------------------------------------------------------------

elixirCategory : Alert -> Category
elixirCategory tokenPermissions        = workflowSecurity
elixirCategory pinnedDependencies      = workflowSecurity
elixirCategory missingWorkflowPerms    = workflowSecurity
elixirCategory hardCodedCrypto         = codeSecurity
elixirCategory remotePropertyInjection = codeSecurity
elixirCategory sqlInjection            = codeSecurity
elixirCategory unusedLocalVariable     = codeQuality
elixirCategory syntaxError             = codeQuality
elixirCategory vulnerabilities         = dependencyVuln
elixirCategory unmaintained            = dependencyVuln
elixirCategory securityPolicy          = processHygiene
elixirCategory maintained              = processHygiene
elixirCategory codeReview              = processHygiene
elixirCategory branchProtection        = processHygiene
elixirCategory ciiBestPractices        = processHygiene
elixirCategory fuzzing                 = missingTests
elixirCategory sast                    = missingTests
elixirCategory ciTests                 = missingTests

elixirSeverity : Alert -> Severity
elixirSeverity tokenPermissions        = high
elixirSeverity pinnedDependencies      = medium
elixirSeverity missingWorkflowPerms    = high
elixirSeverity hardCodedCrypto         = critical
elixirSeverity remotePropertyInjection = high
elixirSeverity sqlInjection            = critical
elixirSeverity unusedLocalVariable     = low
elixirSeverity syntaxError             = medium
elixirSeverity vulnerabilities         = critical
elixirSeverity unmaintained            = low
elixirSeverity securityPolicy          = medium
elixirSeverity maintained              = low
elixirSeverity codeReview              = medium
elixirSeverity branchProtection        = medium
elixirSeverity ciiBestPractices        = low
elixirSeverity fuzzing                 = low
elixirSeverity sast                    = low
elixirSeverity ciTests                 = low

elixirAutoFixable : Alert -> Bool
elixirAutoFixable tokenPermissions        = true
elixirAutoFixable pinnedDependencies      = true
elixirAutoFixable missingWorkflowPerms    = true
elixirAutoFixable hardCodedCrypto         = false
elixirAutoFixable remotePropertyInjection = false
elixirAutoFixable sqlInjection            = false
elixirAutoFixable unusedLocalVariable     = true
elixirAutoFixable syntaxError             = false
elixirAutoFixable vulnerabilities         = false
elixirAutoFixable unmaintained            = false
elixirAutoFixable securityPolicy          = true
elixirAutoFixable maintained              = false
elixirAutoFixable codeReview              = false
elixirAutoFixable branchProtection        = true
elixirAutoFixable ciiBestPractices        = false
elixirAutoFixable fuzzing                 = false
elixirAutoFixable sast                    = false
elixirAutoFixable ciTests                 = false

elixirHasFix : Alert -> Bool
elixirHasFix tokenPermissions        = true
elixirHasFix pinnedDependencies      = true
elixirHasFix missingWorkflowPerms    = true
elixirHasFix hardCodedCrypto         = true
elixirHasFix remotePropertyInjection = true
elixirHasFix sqlInjection            = false
elixirHasFix unusedLocalVariable     = true
elixirHasFix syntaxError             = true
elixirHasFix vulnerabilities         = true
elixirHasFix unmaintained            = false
elixirHasFix securityPolicy          = true
elixirHasFix maintained              = false
elixirHasFix codeReview              = true
elixirHasFix branchProtection        = true
elixirHasFix ciiBestPractices        = true
elixirHasFix fuzzing                 = true
elixirHasFix sast                    = false
elixirHasFix ciTests                 = false

elixirPrevention : Alert -> Maybe Workflow
elixirPrevention tokenPermissions        = just workflowLinter
elixirPrevention pinnedDependencies      = just workflowLinter
elixirPrevention missingWorkflowPerms    = just workflowLinter
elixirPrevention hardCodedCrypto         = just secretScanner
elixirPrevention remotePropertyInjection = nothing
elixirPrevention sqlInjection            = nothing
elixirPrevention unusedLocalVariable     = nothing
elixirPrevention syntaxError             = nothing
elixirPrevention vulnerabilities         = just cargoAudit
elixirPrevention unmaintained            = nothing
elixirPrevention securityPolicy          = just scorecardEnforcer
elixirPrevention maintained              = nothing
elixirPrevention codeReview              = just scorecardEnforcer
elixirPrevention branchProtection        = just scorecardEnforcer
elixirPrevention ciiBestPractices        = nothing
elixirPrevention fuzzing                 = nothing
elixirPrevention sast                    = nothing
elixirPrevention ciTests                 = nothing

------------------------------------------------------------------------
-- Correspondence theorems (exhaustive over the finite Alert type)
------------------------------------------------------------------------

categoriesAgree : (a : Alert) -> logtalkCategory a ≡ elixirCategory a
categoriesAgree tokenPermissions        = refl
categoriesAgree pinnedDependencies      = refl
categoriesAgree missingWorkflowPerms    = refl
categoriesAgree hardCodedCrypto         = refl
categoriesAgree remotePropertyInjection = refl
categoriesAgree sqlInjection            = refl
categoriesAgree unusedLocalVariable     = refl
categoriesAgree syntaxError             = refl
categoriesAgree vulnerabilities         = refl
categoriesAgree unmaintained            = refl
categoriesAgree securityPolicy          = refl
categoriesAgree maintained              = refl
categoriesAgree codeReview              = refl
categoriesAgree branchProtection        = refl
categoriesAgree ciiBestPractices        = refl
categoriesAgree fuzzing                 = refl
categoriesAgree sast                    = refl
categoriesAgree ciTests                 = refl

severitiesAgree : (a : Alert) -> logtalkSeverity a ≡ elixirSeverity a
severitiesAgree tokenPermissions        = refl
severitiesAgree pinnedDependencies      = refl
severitiesAgree missingWorkflowPerms    = refl
severitiesAgree hardCodedCrypto         = refl
severitiesAgree remotePropertyInjection = refl
severitiesAgree sqlInjection            = refl
severitiesAgree unusedLocalVariable     = refl
severitiesAgree syntaxError             = refl
severitiesAgree vulnerabilities         = refl
severitiesAgree unmaintained            = refl
severitiesAgree securityPolicy          = refl
severitiesAgree maintained              = refl
severitiesAgree codeReview              = refl
severitiesAgree branchProtection        = refl
severitiesAgree ciiBestPractices        = refl
severitiesAgree fuzzing                 = refl
severitiesAgree sast                    = refl
severitiesAgree ciTests                 = refl

autoFixableAgrees : (a : Alert) -> logtalkAutoFixable a ≡ elixirAutoFixable a
autoFixableAgrees tokenPermissions        = refl
autoFixableAgrees pinnedDependencies      = refl
autoFixableAgrees missingWorkflowPerms    = refl
autoFixableAgrees hardCodedCrypto         = refl
autoFixableAgrees remotePropertyInjection = refl
autoFixableAgrees sqlInjection            = refl
autoFixableAgrees unusedLocalVariable     = refl
autoFixableAgrees syntaxError             = refl
autoFixableAgrees vulnerabilities         = refl
autoFixableAgrees unmaintained            = refl
autoFixableAgrees securityPolicy          = refl
autoFixableAgrees maintained              = refl
autoFixableAgrees codeReview              = refl
autoFixableAgrees branchProtection        = refl
autoFixableAgrees ciiBestPractices        = refl
autoFixableAgrees fuzzing                 = refl
autoFixableAgrees sast                    = refl
autoFixableAgrees ciTests                 = refl

fixPresenceAgrees : (a : Alert) -> logtalkHasFix a ≡ elixirHasFix a
fixPresenceAgrees tokenPermissions        = refl
fixPresenceAgrees pinnedDependencies      = refl
fixPresenceAgrees missingWorkflowPerms    = refl
fixPresenceAgrees hardCodedCrypto         = refl
fixPresenceAgrees remotePropertyInjection = refl
fixPresenceAgrees sqlInjection            = refl
fixPresenceAgrees unusedLocalVariable     = refl
fixPresenceAgrees syntaxError             = refl
fixPresenceAgrees vulnerabilities         = refl
fixPresenceAgrees unmaintained            = refl
fixPresenceAgrees securityPolicy          = refl
fixPresenceAgrees maintained              = refl
fixPresenceAgrees codeReview              = refl
fixPresenceAgrees branchProtection        = refl
fixPresenceAgrees ciiBestPractices        = refl
fixPresenceAgrees fuzzing                 = refl
fixPresenceAgrees sast                    = refl
fixPresenceAgrees ciTests                 = refl

-- Prevention: agreement on the six unchanged shared entries.
preventionAgreesToken :
  logtalkPrevention tokenPermissions ≡ elixirPrevention tokenPermissions
preventionAgreesToken = refl

preventionAgreesPinned :
  logtalkPrevention pinnedDependencies ≡ elixirPrevention pinnedDependencies
preventionAgreesPinned = refl

preventionAgreesMissing :
  logtalkPrevention missingWorkflowPerms ≡ elixirPrevention missingWorkflowPerms
preventionAgreesMissing = refl

preventionAgreesHardCoded :
  logtalkPrevention hardCodedCrypto ≡ elixirPrevention hardCodedCrypto
preventionAgreesHardCoded = refl

preventionAgreesVulnerabilities :
  logtalkPrevention vulnerabilities ≡ elixirPrevention vulnerabilities
preventionAgreesVulnerabilities = refl

preventionAgreesSecurityPolicy :
  logtalkPrevention securityPolicy ≡ elixirPrevention securityPolicy
preventionAgreesSecurityPolicy = refl

-- Prevention: the two ledgered non-equalities, stated exactly.
preventionBranchSource :
  logtalkPrevention branchProtection ≡ just branchProtectionEnforcer
preventionBranchSource = refl

preventionBranchTarget :
  elixirPrevention branchProtection ≡ just scorecardEnforcer
preventionBranchTarget = refl

preventionCodeReviewSource :
  logtalkPrevention codeReview ≡ nothing
preventionCodeReviewSource = refl

preventionCodeReviewTarget :
  elixirPrevention codeReview ≡ just scorecardEnforcer
preventionCodeReviewTarget = refl

-- The three alerts that reached low only through the Logtalk catch-all
-- are explicit low entries in Elixir. (Also covered by
-- severitiesAgree; named here because this was the one place where a
-- fact-by-fact transcription would have changed behaviour to :info.)
catchAllSastLow : elixirSeverity sast ≡ low
catchAllSastLow = refl

catchAllCiTestsLow : elixirSeverity ciTests ≡ low
catchAllCiTestsLow = refl

catchAllUnmaintainedLow : elixirSeverity unmaintained ≡ low
catchAllUnmaintainedLow = refl
