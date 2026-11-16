{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Finite correspondence model between the source of record for the
-- security-alert rules (ECHIDNA's security-learning rule table) and the
-- Elixir target Hypatia.Rules.SecurityErrors.
--
-- Scope, deliberately narrow (estate PROOF-POLICY: a checked model is not
-- a runtime-refinement proof):
--
--   * The source functions transcribe the source of record: the 18
--     alert_category/2 facts, the classify_severity/2 clauses (including
--     the final low catch-all), the six is_auto_fixable/1 facts, the
--     domain of suggest_fix/2, and get_prevention_workflow/2.
--   * The target functions transcribe the corresponding Elixir tables in
--     lib/rules/security_errors.ex over the same 18 alert types.
--   * The theorems check exhaustive agreement for category, severity,
--     auto-fixability, and fix presence. Prevention agrees on the six
--     shared, unchanged entries; the BranchProtectionID supersession and
--     the CodeReviewID extension are stated as separate lemmas rather
--     than hidden inside a false universal equality.
--   * Fix *text* is not modelled here: 10 of 13 strings are verbatim and
--     the other three are recorded supersessions/generalisation in
--     docs/rules/security-rule-provenance.adoc. Elixir runtime behaviour
--     is pinned by ExUnit tests, not by this file.
--
-- Status and grade (standards PROOF-POLICY §5 and §8): the target is
-- `PROVEN`, `PWG-3`, which holds only while the strict command in
-- verification/PROOF-STATUS.adoc has a recorded successful receipt. The
-- current status and the receipt are recorded there, not here. The grade
-- covers this finite model only: there is no Rocq or Lean leg, so no
-- recheck correspondence state (§2.2) applies, and no runtime-refinement
-- claim is made.
-- No postulates, no unsafe options, no standard-library dependency.

module AuditPreventionCorrespondence where

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
-- Source of record: ECHIDNA security-learning rule table
------------------------------------------------------------------------

sourceCategory : Alert -> Category
sourceCategory tokenPermissions        = workflowSecurity
sourceCategory pinnedDependencies      = workflowSecurity
sourceCategory missingWorkflowPerms    = workflowSecurity
sourceCategory hardCodedCrypto         = codeSecurity
sourceCategory remotePropertyInjection = codeSecurity
sourceCategory sqlInjection            = codeSecurity
sourceCategory unusedLocalVariable     = codeQuality
sourceCategory syntaxError             = codeQuality
sourceCategory vulnerabilities         = dependencyVuln
sourceCategory unmaintained            = dependencyVuln
sourceCategory securityPolicy          = processHygiene
sourceCategory maintained              = processHygiene
sourceCategory codeReview              = processHygiene
sourceCategory branchProtection        = processHygiene
sourceCategory ciiBestPractices        = processHygiene
sourceCategory fuzzing                 = missingTests
sourceCategory sast                    = missingTests
sourceCategory ciTests                 = missingTests

-- The final source clause is classify_severity(_, low), so every alert
-- outside the critical/high/medium member lists is low here.
sourceSeverity : Alert -> Severity
sourceSeverity tokenPermissions        = high
sourceSeverity pinnedDependencies      = medium
sourceSeverity missingWorkflowPerms    = high
sourceSeverity hardCodedCrypto         = critical
sourceSeverity remotePropertyInjection = high
sourceSeverity sqlInjection            = critical
sourceSeverity unusedLocalVariable     = low
sourceSeverity syntaxError             = medium
sourceSeverity vulnerabilities         = critical
sourceSeverity unmaintained            = low
sourceSeverity securityPolicy          = medium
sourceSeverity maintained              = low
sourceSeverity codeReview              = medium
sourceSeverity branchProtection        = medium
sourceSeverity ciiBestPractices        = low
sourceSeverity fuzzing                 = low
sourceSeverity sast                    = low
sourceSeverity ciTests                 = low

sourceAutoFixable : Alert -> Bool
sourceAutoFixable tokenPermissions        = true
sourceAutoFixable pinnedDependencies      = true
sourceAutoFixable missingWorkflowPerms    = true
sourceAutoFixable hardCodedCrypto         = false
sourceAutoFixable remotePropertyInjection = false
sourceAutoFixable sqlInjection            = false
sourceAutoFixable unusedLocalVariable     = true
sourceAutoFixable syntaxError             = false
sourceAutoFixable vulnerabilities         = false
sourceAutoFixable unmaintained            = false
sourceAutoFixable securityPolicy          = true
sourceAutoFixable maintained              = false
sourceAutoFixable codeReview              = false
sourceAutoFixable branchProtection        = true
sourceAutoFixable ciiBestPractices        = false
sourceAutoFixable fuzzing                 = false
sourceAutoFixable sast                    = false
sourceAutoFixable ciTests                 = false

-- Domain of suggest_fix/2 only (13 facts); the strings themselves are
-- handled by the provenance record and Elixir tests.
sourceHasFix : Alert -> Bool
sourceHasFix tokenPermissions        = true
sourceHasFix pinnedDependencies      = true
sourceHasFix missingWorkflowPerms    = true
sourceHasFix hardCodedCrypto         = true
sourceHasFix remotePropertyInjection = true
sourceHasFix sqlInjection            = false
sourceHasFix unusedLocalVariable     = true
sourceHasFix syntaxError             = true
sourceHasFix vulnerabilities         = true
sourceHasFix unmaintained            = false
sourceHasFix securityPolicy          = true
sourceHasFix maintained              = false
sourceHasFix codeReview              = true
sourceHasFix branchProtection        = true
sourceHasFix ciiBestPractices        = true
sourceHasFix fuzzing                 = true
sourceHasFix sast                    = false
sourceHasFix ciTests                 = false

sourcePrevention : Alert -> Maybe Workflow
sourcePrevention tokenPermissions        = just workflowLinter
sourcePrevention pinnedDependencies      = just workflowLinter
sourcePrevention missingWorkflowPerms    = just workflowLinter
sourcePrevention hardCodedCrypto         = just secretScanner
sourcePrevention remotePropertyInjection = nothing
sourcePrevention sqlInjection            = nothing
sourcePrevention unusedLocalVariable     = nothing
sourcePrevention syntaxError             = nothing
sourcePrevention vulnerabilities         = just cargoAudit
sourcePrevention unmaintained            = nothing
sourcePrevention securityPolicy          = just scorecardEnforcer
sourcePrevention maintained              = nothing
sourcePrevention codeReview              = nothing
sourcePrevention branchProtection        = just branchProtectionEnforcer
sourcePrevention ciiBestPractices        = nothing
sourcePrevention fuzzing                 = nothing
sourcePrevention sast                    = nothing
sourcePrevention ciTests                 = nothing

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

categoriesAgree : (a : Alert) -> sourceCategory a ≡ elixirCategory a
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

severitiesAgree : (a : Alert) -> sourceSeverity a ≡ elixirSeverity a
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

autoFixableAgrees : (a : Alert) -> sourceAutoFixable a ≡ elixirAutoFixable a
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

fixPresenceAgrees : (a : Alert) -> sourceHasFix a ≡ elixirHasFix a
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
  sourcePrevention tokenPermissions ≡ elixirPrevention tokenPermissions
preventionAgreesToken = refl

preventionAgreesPinned :
  sourcePrevention pinnedDependencies ≡ elixirPrevention pinnedDependencies
preventionAgreesPinned = refl

preventionAgreesMissing :
  sourcePrevention missingWorkflowPerms ≡ elixirPrevention missingWorkflowPerms
preventionAgreesMissing = refl

preventionAgreesHardCoded :
  sourcePrevention hardCodedCrypto ≡ elixirPrevention hardCodedCrypto
preventionAgreesHardCoded = refl

preventionAgreesVulnerabilities :
  sourcePrevention vulnerabilities ≡ elixirPrevention vulnerabilities
preventionAgreesVulnerabilities = refl

preventionAgreesSecurityPolicy :
  sourcePrevention securityPolicy ≡ elixirPrevention securityPolicy
preventionAgreesSecurityPolicy = refl

-- Prevention: the two recorded non-equalities, stated exactly.
preventionBranchSource :
  sourcePrevention branchProtection ≡ just branchProtectionEnforcer
preventionBranchSource = refl

preventionBranchTarget :
  elixirPrevention branchProtection ≡ just scorecardEnforcer
preventionBranchTarget = refl

preventionCodeReviewSource :
  sourcePrevention codeReview ≡ nothing
preventionCodeReviewSource = refl

preventionCodeReviewTarget :
  elixirPrevention codeReview ≡ just scorecardEnforcer
preventionCodeReviewTarget = refl

-- The three alerts that reached low only through the source catch-all
-- are explicit low entries in Elixir. (Also covered by
-- severitiesAgree; named here because this was the one place where a
-- fact-by-fact transcription would have changed behaviour to :info.)
catchAllSastLow : elixirSeverity sast ≡ low
catchAllSastLow = refl

catchAllCiTestsLow : elixirSeverity ciTests ≡ low
catchAllCiTestsLow = refl

catchAllUnmaintainedLow : elixirSeverity unmaintained ≡ low
catchAllUnmaintainedLow = refl
