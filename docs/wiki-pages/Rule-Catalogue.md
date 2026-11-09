<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Rule Catalogue

Every rule module Hypatia ships. **Generated** from `lib/rules/*.ex` — regenerate with
`./scripts/gen-rule-catalogue.sh` in the repo, then re-publish this page.


| Module | Rule IDs | Purpose |
|---|---|---|
| `actions_lock` | — | Strict local validation for machine-generated `gh actions-lock` v0.0.2/v0.0.3 files. |
| `admin_merge_eligibility` | DBA002 | Recognises PR shapes that are safe to admin-merge in bulk when the |
| `audit_prevention` | AP001 AP002 AP003 AP004 AP005 AP006 AP007 AP008 AP009 AP010 AP011 AP012 AP013 AP014 AP015 | Audit-training prevention rules AP001–AP015, transcribed from Logtalk. |
| `baseline_health` | BH001 BH002 BH003 BH004 BH005 BH006 BH007 BH008 | Detects degraded `main`-branch baseline conditions that allow silent rot. |
| `branch_protection` | BP001 BP002 BP003 BP004 BP005 BP006 BP007 BP008 BP009 | Branch-protection hygiene rules drawn from the CIS GitHub Benchmark, |
| `build_system_rules` | — | Build-tool configuration anti-patterns that block estate CI but slip past |
| `cicd_rules` | — | Primary Elixir CI/CD policy rules (migrated from legacy Logtalk engine). |
| `code_safety` | — | Primary Elixir rules for code safety (migrated from legacy Logtalk engine). |
| `code_scanning_alerts` | CSA001 CSA002 CSA003 CSA004 CSA005 CSA006 | GitHub Code Scanning alert querying (CodeQL + third-party SARIF). |
| `dependabot_alerts` | DA001 DA002 DA003 DA004 | GitHub Dependabot alert querying and risk assessment. |
| `disambiguation_rules` | DR001 | Cross-reference / language-disambiguation rules. |
| `dogfooding` | HYP-DOG-001 HYP-DOG-003 HYP-DOG-004 HYP-DOG-005 HYP-DOG-006 HYP-DOG-007 HYP-DOG-008 HYP-DOG-009 HYP-DOG-010 | Dogfooding compliance rules -- HYP-DOG-001 through HYP-DOG-010. |
| `forge_adapters` | — | Primary Elixir forge adapter rules (migrated from legacy Logtalk engine). |
| `git_state` | GS001 GS002 GS003 GS004 GS005 GS006 GS007 GS008 | Git repository synchronisation state checks. |
| `green_web` | — | Green Web Foundation certification checks for hyperpolymath repositories. |
| `honest_completion` | — | Honest completion audit -- cross-references claimed completion percentages |
| `implementation_inside_canon` | — | Executable evaluator for the standards-authored HYP-S009 rule. |
| `learning` | — | Learning engine rules absorbed from the retired Logtalk learning and |
| `migration_rules` | — | Primary Elixir ReScript migration rules (migrated from legacy Logtalk engine). |
| `moved_repositories` | — | Versioned records of confirmed repository moves relevant to reusable workflows. |
| `pin_integrity` | — | Pin-level integrity rules for workflow and lockfile content. |
| `pin_sites` | — | Structural `uses:` pin sites on one line of workflow YAML. |
| `pr_automerge` | — | Decide whether a pull request is an unambiguous bump or chore that may be |
| `proof_obligation` | — | ProofObligation recipe type for Hypatia. |
| `proof_strategy_selection` | — | Prover-strategy selection from VeriSimDB historical outcomes. |
| `research_extensions` | RE001 RE002 RE003 RE004 RE005 RE006 RE007 RE008 RE009 RE010 | Static detection of GitHub Actions workflow defects sourced from |
| `root_hygiene` | ERR-GIT-001 | Root directory hygiene enforcement rules. |
| `rsr_conformance` | — | The RSR v2.0 conformance **oracle** (increment 3 of the HYP-S family): |
| `rsr_criteria` | — | Loads the **RSR v2.0 criteria SSOT** (`rsr-criteria-v2.a2ml` from |
| `rule_loader` | — | Loads standards-authored rule definitions (`.a2ml` files) into structured |
| `rules` | ERR-LIC-001 | Facade module for all Hypatia rules, absorbed from the Logtalk engine. |
| `scorecard_compliance` | — | Enforces OpenSSF Scorecard best practices as automated Elixir rules. |
| `secret_scanning_alerts` | SSA001 SSA002 SSA003 SSA004 | GitHub Secret Scanning alert querying. |
| `security_errors` | — | Primary Elixir security error database (migrated from legacy Logtalk engine). |
| `sha_bump_propagation` | — | Detection: an estate-wide reusable workflow has been bumped to a new SHA |
| `standards_repo` | — | The names of the estate standards repository. |
| `strategy_drift` | — | N4: Detects strategy-shift events and flags previously-failed attempts |
| `structural_drift` | SD001 SD002 SD003 SD004 SD005 SD006 SD007 SD008 SD009 SD010 SD011 SD013 SD014 SD021 SD022 SD023 SD024 | Structural drift detection rules. |
| `supply_chain` | SC001 SC002 SC003 SC004 SC005 SC006 SC007 SC008 SC009 SC010 SC011 SC012 SC013 | Supply-chain integrity rules drawn from OSSF Scorecard, SLSA, OWASP |
| `workflow_audit` | ERR-WF-013 ERR-WF-014 WF-025 WF013 WF014 WF015 WF016 WF017 WF018 WF019 WF020 WF021 WF022 WF023 WF024 WF025 WF026 WF027 WF028 | CI/CD workflow audit rules. |
| `workflow_hardening` | WH001 WH002 WH003 WH004 WH005 WH006 WH007 WH008 WH009 WH010 WH011 WH012 WH013 WH014 | Static detection of dangerous GitHub Actions workflow patterns. |

## ID families

| Prefix | Family |
|---|---|
| `AM` | Admin-merge eligibility |
| `AP` | Audit-prevention rules transcribed from the historical Logtalk blocks |
| `BH` | Baseline health (silent `main` rot) |
| `BP` | Branch protection (CIS / Scorecard / NIST SSDF) |
| `CSA` | Code-scanning alerts (CodeQL + third-party SARIF) |
| `DA` | Dependabot alerts |
| `GS` | Git synchronisation state |
| `HYP-DOG` | Dogfooding compliance |
| `RE` | Research extensions (Snyk / StepSecurity-sourced defects) |
| `SC` | Supply chain (Scorecard / SLSA / OWASP CI-CD Top-10) |
| `SD` | Structural drift |
| `SSA` | Secret-scanning alerts |
| `WF` / `ERR-WF` | Workflow audit |
| `WH` | Workflow hardening |

## Suppressing a finding

In order of preference: **fix the code** > inline `hypatia: allow <module>/<rule> -- reason`
directive at the call site > `.hypatia-ignore` (file/dir scoped, needs a documented policy
rationale) > `.hypatia-baseline.json` (last resort — baseline entries are accepted risk that
every future reader inherits).
