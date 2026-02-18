{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
-- SPDX-License-Identifier: PMPL-1.0-or-later
-- | Type-safe ruleset DSL for cicd-hyper-a
--
-- This module provides a GADT-based DSL for defining CI/CD rules with
-- three effect types:
--
--   * 'Preventive' - Block commits that violate policies
--   * 'Curative' - Fix existing issues in repositories
--   * 'Diagnostic' - Report issues without modification
--
-- Rules are composed into 'Ruleset's with metadata for versioning,
-- provenance tracking, and formal verification.

module Hypatia.Ruleset
  ( -- * Effect types
    Effect(..)
    -- * Rule types
  , Rule(..)
  , RuleName
  , Condition(..)
  , Action(..)
  , Pattern
  , Fix(..)
  , Check(..)
  , Report(..)
    -- * Language types
  , Language(..)
    -- * Severity
  , Severity(..)
    -- * Ruleset container
  , Ruleset(..)
  , RulesetMetadata(..)
  , RulesetCategory(..)
  , TargetLanguage(..)
  , SomeRule(..)
    -- * Ruleset operations
  , mkRuleset
  , addRule
  , removeRule
  , filterByEffect
  , filterByCategory
  , getRuleCount
  , getRuleName
  , getRuleEffect
    -- * Pre-built rules
  , requireDependabot
  , requireSecurityMd
  , blockTypeScript
  , blockGolang
  , pinGitHubActions
  , requireWorkflowPermissions
  , requireSpdxHeader
  , requireCodeOwners
  , blockPython
  , blockNodeModules
  , requireBranchProtection
    -- * Pre-built rulesets
  , rsrComplianceRuleset
  , securityBaselineRuleset
  , openssfScorecard
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Rule effect types
data Effect = Preventive | Curative | Diagnostic
  deriving (Show, Eq)

-- | Severity levels (from 1922 alert analysis)
data Severity = Critical | High | Medium | Low | Info
  deriving (Show, Eq, Ord)

type RuleName = Text
type Pattern = Text
type FilePath' = Text

-- | Type-safe rule definition
data Rule (e :: Effect) where
  PreventiveRule :: RuleName -> Condition -> Action -> Rule 'Preventive
  CurativeRule   :: RuleName -> Pattern -> Fix -> Rule 'Curative
  DiagnosticRule :: RuleName -> Check -> Report -> Rule 'Diagnostic

deriving instance Show (Rule e)

-- | Condition language for preventive rules
data Condition
  = FileExists FilePath'
  | FileContains FilePath' Pattern
  | FileExtension FilePath' Text
  | LanguageUsed Language
  | DependencyPresent Text
  | WorkflowHas Pattern
  | And Condition Condition
  | Or Condition Condition
  | Not Condition
  deriving (Show, Eq)

-- | Languages
data Language
  = TypeScript | JavaScript | Golang | Python | Rust
  | ReScript | Gleam | Julia | Haskell | Logtalk
  | Nickel | Guile | OCaml | Ada
  deriving (Show, Eq)

-- | Actions for preventive rules
data Action
  = InjectFile FilePath' Text
  | RejectCommit Text
  | RequireApproval Text
  | AddToWorkflow Text
  | Alert Severity Text
  deriving (Show, Eq)

-- | Fixes for curative rules
data Fix
  = ReplaceInFile FilePath' Pattern Text
  | DeleteFile FilePath'
  | AddFile FilePath' Text
  | RunCommand Text
  | CreatePR Text Text  -- title, body
  deriving (Show, Eq)

-- | Checks for diagnostic rules
data Check
  = CheckFileExists FilePath'
  | CheckPattern FilePath' Pattern
  | CheckCommand Text
  | CheckApi Text
  deriving (Show, Eq)

-- | Reports for diagnostic rules
data Report
  = ReportToConsole
  | ReportToFile FilePath'
  | ReportToApi Text
  | ReportToPR
  deriving (Show, Eq)

-- ============================================================
-- Pre-built Rules (distilled from 228 repos)
-- ============================================================

-- | Require dependabot.yml for repos with dependencies
requireDependabot :: Rule 'Preventive
requireDependabot = PreventiveRule
  "require-dependabot"
  (Not (FileExists ".github/dependabot.yml"))
  (InjectFile ".github/dependabot.yml" dependabotTemplate)

-- | Require SECURITY.md for public repos
requireSecurityMd :: Rule 'Preventive
requireSecurityMd = PreventiveRule
  "require-security-md"
  (Not (FileExists "SECURITY.md"))
  (InjectFile "SECURITY.md" securityMdTemplate)

-- | Block TypeScript (RSR policy)
blockTypeScript :: Rule 'Preventive
blockTypeScript = PreventiveRule
  "block-typescript"
  (Or (LanguageUsed TypeScript) (FileExtension "*" ".ts"))
  (RejectCommit "TypeScript not allowed per RSR policy. Use ReScript instead.")

-- | Block Golang (RSR policy)
blockGolang :: Rule 'Preventive
blockGolang = PreventiveRule
  "block-golang"
  (Or (LanguageUsed Golang) (FileExtension "*" ".go"))
  (RejectCommit "Go not allowed per RSR policy. Use Rust instead.")

-- | Pin GitHub Actions to SHA
pinGitHubActions :: Rule 'Curative
pinGitHubActions = CurativeRule
  "pin-github-actions"
  "uses:\\s+([^@]+)@(v\\d+|main|master)"
  (ReplaceInFile ".github/workflows/*.yml"
    "uses: $1@$2"
    "uses: $1@SHA # $2")

-- | Require workflow permissions
requireWorkflowPermissions :: Rule 'Preventive
requireWorkflowPermissions = PreventiveRule
  "require-workflow-permissions"
  (And
    (FileExists ".github/workflows/*.yml")
    (Not (WorkflowHas "permissions:")))
  (AddToWorkflow "permissions: read-all")

-- | Require SPDX headers
requireSpdxHeader :: Rule 'Preventive
requireSpdxHeader = PreventiveRule
  "require-spdx-header"
  (Not (FileContains "*" "SPDX-License-Identifier"))
  (Alert Medium "File missing SPDX license header")

-- ============================================================
-- Templates
-- ============================================================

dependabotTemplate :: Text
dependabotTemplate = T.unlines
  [ "# SPDX-License-Identifier: PMPL-1.0-or-later"
  , "version: 2"
  , "updates:"
  , "  - package-ecosystem: \"github-actions\""
  , "    directory: \"/\""
  , "    schedule:"
  , "      interval: \"weekly\""
  ]

securityMdTemplate :: Text
securityMdTemplate = T.unlines
  [ "# Security Policy"
  , ""
  , "## Reporting a Vulnerability"
  , ""
  , "Please report security vulnerabilities to security@hyperpolymath.dev"
  , ""
  , "We will respond within 48 hours and provide a fix within 7 days for critical issues."
  ]

-- ============================================================
-- Ruleset Container
-- ============================================================

-- | Existential wrapper for heterogeneous rule collections
data SomeRule = forall e. SomeRule (Rule e)

instance Show SomeRule where
  show (SomeRule r) = show r

-- | Ruleset category for classification
data RulesetCategory
  = SecurityCategory        -- ^ Security-related rules
  | ComplianceCategory      -- ^ Policy compliance rules
  | QualityCategory         -- ^ Code quality rules
  | PerformanceCategory     -- ^ Performance-related rules
  | DocumentationCategory   -- ^ Documentation requirements
  | InfrastructureCategory  -- ^ CI/CD infrastructure rules
  deriving (Show, Eq, Ord, Generic)

-- | Target language specification
data TargetLanguage
  = AllLanguages            -- ^ Applies to all languages
  | SpecificLanguages [Language]  -- ^ Applies only to specific languages
  deriving (Show, Eq, Generic)

-- | Ruleset metadata for versioning and provenance
data RulesetMetadata = RulesetMetadata
  { metaName         :: Text           -- ^ Unique ruleset identifier
  , metaVersion      :: (Int, Int, Int) -- ^ Semantic version (major, minor, patch)
  , metaDescription  :: Text           -- ^ Human-readable description
  , metaAuthor       :: Text           -- ^ Author or organization
  , metaLicense      :: Text           -- ^ SPDX license identifier
  , metaCategory     :: RulesetCategory -- ^ Primary category
  , metaTargets      :: TargetLanguage  -- ^ Target languages
  , metaCreated      :: Maybe UTCTime   -- ^ Creation timestamp
  , metaUpdated      :: Maybe UTCTime   -- ^ Last update timestamp
  , metaVerified     :: Bool            -- ^ Formally verified flag
  , metaSigned       :: Maybe Text      -- ^ GPG signature
  , metaTags         :: [Text]          -- ^ Searchable tags
  , metaDepends      :: [Text]          -- ^ Dependencies on other rulesets
  } deriving (Show, Eq, Generic)

-- | Complete ruleset with metadata and rules
data Ruleset = Ruleset
  { rulesetMeta       :: RulesetMetadata  -- ^ Metadata
  , rulesetPreventive :: [Rule 'Preventive] -- ^ Preventive rules
  , rulesetCurative   :: [Rule 'Curative]   -- ^ Curative rules
  , rulesetDiagnostic :: [Rule 'Diagnostic] -- ^ Diagnostic rules
  } deriving (Show)

-- ============================================================
-- Ruleset Operations
-- ============================================================

-- | Create a new empty ruleset with metadata
mkRuleset :: Text -> Text -> Text -> RulesetCategory -> Ruleset
mkRuleset name desc author cat = Ruleset
  { rulesetMeta = RulesetMetadata
      { metaName = name
      , metaVersion = (1, 0, 0)
      , metaDescription = desc
      , metaAuthor = author
      , metaLicense = "PMPL-1.0-or-later"
      , metaCategory = cat
      , metaTargets = AllLanguages
      , metaCreated = Nothing
      , metaUpdated = Nothing
      , metaVerified = False
      , metaSigned = Nothing
      , metaTags = []
      , metaDepends = []
      }
  , rulesetPreventive = []
  , rulesetCurative = []
  , rulesetDiagnostic = []
  }

-- | Add a rule to a ruleset
addRule :: SomeRule -> Ruleset -> Ruleset
addRule (SomeRule rule) rs = case rule of
  PreventiveRule{} -> rs { rulesetPreventive = rule : rulesetPreventive rs }
  CurativeRule{} -> rs { rulesetCurative = rule : rulesetCurative rs }
  DiagnosticRule{} -> rs { rulesetDiagnostic = rule : rulesetDiagnostic rs }

-- | Remove a rule by name
removeRule :: Text -> Ruleset -> Ruleset
removeRule name rs = rs
  { rulesetPreventive = filter ((/= name) . getRuleName) (rulesetPreventive rs)
  , rulesetCurative = filter ((/= name) . getRuleName) (rulesetCurative rs)
  , rulesetDiagnostic = filter ((/= name) . getRuleName) (rulesetDiagnostic rs)
  }

-- | Filter rules by effect type
filterByEffect :: Effect -> Ruleset -> [SomeRule]
filterByEffect Preventive rs = map SomeRule (rulesetPreventive rs)
filterByEffect Curative rs = map SomeRule (rulesetCurative rs)
filterByEffect Diagnostic rs = map SomeRule (rulesetDiagnostic rs)

-- | Filter rulesets by category
filterByCategory :: RulesetCategory -> [Ruleset] -> [Ruleset]
filterByCategory cat = filter ((== cat) . metaCategory . rulesetMeta)

-- | Get total rule count
getRuleCount :: Ruleset -> Int
getRuleCount rs =
  length (rulesetPreventive rs) +
  length (rulesetCurative rs) +
  length (rulesetDiagnostic rs)

-- | Extract rule name from any rule type
getRuleName :: Rule e -> Text
getRuleName (PreventiveRule n _ _) = n
getRuleName (CurativeRule n _ _) = n
getRuleName (DiagnosticRule n _ _) = n

-- | Get the effect type of a rule
getRuleEffect :: Rule e -> Effect
getRuleEffect PreventiveRule{} = Preventive
getRuleEffect CurativeRule{} = Curative
getRuleEffect DiagnosticRule{} = Diagnostic

-- ============================================================
-- Additional Pre-built Rules
-- ============================================================

-- | Require CODEOWNERS file
requireCodeOwners :: Rule 'Preventive
requireCodeOwners = PreventiveRule
  "require-codeowners"
  (Not (Or (FileExists "CODEOWNERS")
           (FileExists ".github/CODEOWNERS")))
  (InjectFile ".github/CODEOWNERS" codeOwnersTemplate)

-- | Block Python files (RSR policy)
blockPython :: Rule 'Preventive
blockPython = PreventiveRule
  "block-python"
  (Or (LanguageUsed Python) (FileExtension "*" ".py"))
  (RejectCommit "Python not allowed per RSR policy. Use Julia for batch, Rust for systems, ReScript for apps.")

-- | Block node_modules directory
blockNodeModules :: Rule 'Preventive
blockNodeModules = PreventiveRule
  "block-node-modules"
  (FileExists "node_modules")
  (RejectCommit "node_modules not allowed. Use Deno instead of npm.")

-- | Require branch protection configuration
requireBranchProtection :: Rule 'Diagnostic
requireBranchProtection = DiagnosticRule
  "require-branch-protection"
  (CheckApi "repos/{owner}/{repo}/branches/main/protection")
  ReportToConsole

-- ============================================================
-- Pre-built Rulesets
-- ============================================================

-- | RSR (Rhodium Standard Repository) compliance ruleset
rsrComplianceRuleset :: Ruleset
rsrComplianceRuleset = Ruleset
  { rulesetMeta = RulesetMetadata
      { metaName = "hyperpolymath/rsr-compliance"
      , metaVersion = (1, 2, 0)
      , metaDescription = "Rhodium Standard Repository policy enforcement"
      , metaAuthor = "hyperpolymath"
      , metaLicense = "PMPL-1.0-or-later"
      , metaCategory = ComplianceCategory
      , metaTargets = AllLanguages
      , metaCreated = Nothing
      , metaUpdated = Nothing
      , metaVerified = True
      , metaSigned = Nothing
      , metaTags = ["rsr", "language-policy", "compliance"]
      , metaDepends = []
      }
  , rulesetPreventive =
      [ blockTypeScript
      , blockGolang
      , blockPython
      , blockNodeModules
      , requireSpdxHeader
      ]
  , rulesetCurative = []
  , rulesetDiagnostic = []
  }

-- | Security baseline ruleset
securityBaselineRuleset :: Ruleset
securityBaselineRuleset = Ruleset
  { rulesetMeta = RulesetMetadata
      { metaName = "hyperpolymath/security-baseline"
      , metaVersion = (1, 1, 0)
      , metaDescription = "Security hardening rules for all repositories"
      , metaAuthor = "hyperpolymath"
      , metaLicense = "PMPL-1.0-or-later"
      , metaCategory = SecurityCategory
      , metaTargets = AllLanguages
      , metaCreated = Nothing
      , metaUpdated = Nothing
      , metaVerified = True
      , metaSigned = Nothing
      , metaTags = ["security", "hardening", "openssf"]
      , metaDepends = []
      }
  , rulesetPreventive =
      [ requireDependabot
      , requireSecurityMd
      , requireWorkflowPermissions
      , requireCodeOwners
      ]
  , rulesetCurative =
      [ pinGitHubActions
      ]
  , rulesetDiagnostic =
      [ requireBranchProtection
      ]
  }

-- | OpenSSF Scorecard fixes ruleset
openssfScorecard :: Ruleset
openssfScorecard = Ruleset
  { rulesetMeta = RulesetMetadata
      { metaName = "hyperpolymath/openssf-scorecard"
      , metaVersion = (1, 0, 0)
      , metaDescription = "Fixes for OpenSSF Scorecard checks"
      , metaAuthor = "hyperpolymath"
      , metaLicense = "PMPL-1.0-or-later"
      , metaCategory = SecurityCategory
      , metaTargets = AllLanguages
      , metaCreated = Nothing
      , metaUpdated = Nothing
      , metaVerified = True
      , metaSigned = Nothing
      , metaTags = ["openssf", "scorecard", "security", "supply-chain"]
      , metaDepends = ["hyperpolymath/security-baseline"]
      }
  , rulesetPreventive =
      [ requireDependabot
      , requireSecurityMd
      , requireWorkflowPermissions
      ]
  , rulesetCurative =
      [ pinGitHubActions
      ]
  , rulesetDiagnostic =
      [ requireBranchProtection
      ]
  }

-- ============================================================
-- Additional Templates
-- ============================================================

codeOwnersTemplate :: Text
codeOwnersTemplate = T.unlines
  [ "# SPDX-License-Identifier: PMPL-1.0-or-later"
  , "# Default code owners for this repository"
  , "*  @hyperpolymath"
  ]
