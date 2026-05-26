<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `hypatia` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(rules): AffineScript hand-port pitfalls — HANDLE-as-fn-name + OCaml float ops (#332)
- feat(rules): wire 4 new rule modules through the facade (#326)
- feat(rules): ResearchExtensions (RE001-RE010) — 10 rules from Snyk/StepSecurity/Endor/academic literature (#325)
- feat(rules): BranchProtection (BP001-BP007) — 7 rules from CIS GH / Scorecard / NIST SSDF (#323)
- feat(rules): SupplyChain (SC001-SC011) — 11 rules from Scorecard/SLSA/OWASP/Endor (#322)
- feat(rules): WorkflowHardening (WH001-WH012) — 12 rules from actionlint/zizmor/literature (#321)
- feat(rules): BaselineHealth BH004-BH007 — four follow-on baseline-rot detectors (#320)
- feat(rules): BuildSystemRules — cross-repo lint from affinescript#361 (#317)
- feat(rules): lang-policy refresh 2026-05-25 — ban ReScript, retarget TS, reject MPL-1.0 (#318)
- feat(rules): BaselineHealth (BH001-BH003) — detect red-baseline conditions estate-wide (#316)

### Fixed

- fix(supply_chain): replace exponential Levenshtein with iterative DP (#329)
- fix(code_scanning_alerts): break CSA001 self-referential echo loop (#328)
- fix(self): clear 113 dogfood findings — exempt fixtures + harden 9 prod unwraps (#324)
- fix(pkg): retire Nix-mirror per Guix-primary ruling (standards#101) (#289)
- fix(governance)!: banned_language ban is total — no exceptions (#280)
- fix(reconcile): --verify no longer crashes JSON-encoding its result (#272)
- fix(scanner): rebuild stale escript to prevent silent false negatives (#278)
- fix(neural): wire LearningScheduler→force_cycle + RBF restore at init (#275)
- fix(ci): bump a2ml/k9-validate-action pins to canonical (#269)
- fix(ci): sync hypatia-scan.yml to canonical (#268)

### Documentation

- docs: second-pass bucketing — security/, standards/, specs/ (#319)
- docs: tidy root + adopt rsr-template-repo doc taxonomy (#315)
- docs(arch): add as-built MOF M2 metamodel + TOGAF overview (#273 gap 4) (#291)
- docs(arch): add boundary-design-options.adoc (#273 gap 2, decision-ready) (#295)

### CI

- ci: dedupe scheduled-cron triggers + drop dead branch refs in tests.yml + verify-proofs.yml (#331)
- ci: redistribute concurrency-cancel guard to read-only check workflows (#277)
- ci(secret-scanner): drop duplicate --fail from trufflehog extra_args (#227)
- ci(gossamer): cover all test/gossamer/*.test.mjs in loader-smoke (#225)

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
