<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-03-06 -->

# Hypatia — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │           EXTERNAL SCANNERS              │
                        │  panic-attack assail  →  JSON results    │
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │          VERISIMDB-DATA (canonical)      │
                        │  scans/*.json  patterns/  recipes/       │
                        │  outcomes/*.jsonl  index.json             │
                        └───────────────────┬─────────────────────┘
                                            │ VQL + file I/O
                                            ▼
 ┌──────────────────────────────────────────────────────────────────────────┐
 │                         HYPATIA PLATFORM (Elixir/OTP)                   │
 │                                                                          │
 │  ┌────────────────┐   ┌──────────────┐   ┌──────────────────────────┐  │
 │  │ VQL Client     │   │ Pattern      │   │ Recipe Matcher           │  │
 │  │ (query cache)  │──►│ Registry     │──►│ (fuzzy + language infer) │  │
 │  └────────────────┘   └──────┬───────┘   └────────────┬─────────────┘  │
 │                              │                         │                │
 │                              ▼                         ▼                │
 │                    ┌──────────────────┐   ┌────────────────────────┐    │
 │                    │ Triangle Router  │   │  Neural Coordinator    │    │
 │                    │ Eliminate >      │◄──│  (hub-and-spoke)       │    │
 │                    │ Substitute >     │   │                        │    │
 │                    │ Control          │   │  ┌─────┐ ┌─────┐      │    │
 │                    └────────┬─────────┘   │  │ MoE │ │ RBF │      │    │
 │                             │             │  └──┬──┘ └──┬──┘      │    │
 │                             │             │  ┌──▼──┐ ┌──▼──┐      │    │
 │                             │             │  │ LSM │ │ ESN │      │    │
 │                             │             │  └─────┘ └─────┘      │    │
 │                             │             │  ┌───────────┐        │    │
 │                             │             │  │Graph Trust │        │    │
 │                             │             │  └───────────┘        │    │
 │                             │             └────────────────────────┘    │
 │                             ▼                                           │
 │                    ┌──────────────────┐                                 │
 │                    │ Fleet Dispatcher │                                 │
 │                    │ (file + HTTP)    │                                 │
 │                    └────────┬─────────┘                                 │
 │                             │                                           │
 │  ┌──────────┐  ┌──────────┐│ ┌──────────┐  ┌──────────┐  ┌────────┐  │
 │  │ Rate     │  │ Quarant- ││ │ Batch    │  │ Learning │  │ Self   │  │
 │  │ Limiter  │  │ ine      ││ │ Rollback │  │ Scheduler│  │ Diag   │  │
 │  └──────────┘  └──────────┘│ └──────────┘  └──────────┘  └────────┘  │
 └─────────────────────────────┼──────────────────────────────────────────┘
                               │ dispatch manifest (JSONL)
                               ▼
                    ┌──────────────────────┐
                    │    GITBOT-FLEET      │
                    │  dispatch-runner.sh  │
                    │  7 fix scripts       │
                    │  robot-repo-automaton│
                    │  (5% git workflow)   │
                    └──────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE PIPELINE
  VQL Client + File Executor       ██████████ 100%    Built-in parser, 60s cache
  Pattern Registry                 ██████████ 100%    Code complete, needs data seeding
  Recipe Matcher                   ██████████ 100%    Fuzzy matching + language inference
  Triangle Router                  ██████████ 100%    Eliminate > Substitute > Control
  Fleet Dispatcher                 █████████░  90%    File dispatch OK, HTTP graceful
  Dispatch Manifest                ██████████ 100%    JSONL bridge to bash execution
  Pattern Analyzer                 ██████████ 100%    Full pipeline orchestrator

NEURAL SUBSYSTEM (hub-and-spoke)
  Neural Coordinator               █████████░  90%    Wired into pipeline, needs data
  Graph of Trust (PageRank)        █████████░  90%    Code complete, needs outcomes
  Mixture of Experts               ██████████ 100%    7 expert domains + gating
  Liquid State Machine             ██████████ 100%    Temporal anomaly detection
  Echo State Network               ████████░░  80%    Code complete, untrained (no data)
  Radial Basis Function            ████████░░  80%    Code complete, untrained (no data)
  Training Pipeline                ██████████ 100%    Reads verisimdb-data automatically
  Persistence                      ██████████ 100%    ArangoDB + flat file backup

RULES ENGINE (Elixir — absorbed from Logtalk 2026-03-06)
  security_errors.ex               ██████████ 100%    SHA pins, secrets, CWEs
  cicd_rules.ex                    ██████████ 100%    Commit blocking, waste detection
  code_safety.ex                   ██████████ 100%    Per-language dangerous patterns
  migration_rules.ex               ██████████ 100%    ReScript API migration
  learning.ex                      ██████████ 100%    GenServer: confidence scoring
  forge_adapters.ex                ██████████ 100%    Forge ops with input validation

LEARNING & SAFETY
  Outcome Tracker                  ██████████ 100%    Records outcomes, updates confidence
  Learning Scheduler               ██████████ 100%    Polls every 5 min
  Rate Limiter                     ██████████ 100%    50/min/bot, 200/min global
  Quarantine                       ██████████ 100%    Auto on 5+ failures or >30% FP
  Batch Rollback                   ██████████ 100%    Confidence revert capability

OPERATIONAL SCRIPTS
  auto-fix-formulaic.sh            ██████████ 100%    SHA pin, permissions, AGPL, --push
  bot-accountability.sh            ██████████ 100%    .hypatia/activity.jsonl per repo
  systemd timer                    ██████████ 100%    Weekly auto-fix schedule

DATA LAYER
  VeriSimDB Connector              ██████████ 100%    VQL + file I/O fallback
  hypatia-local verisimdb data     ░░░░░░░░░░   0%    EMPTY — needs seeding from fleet

RUST WORKSPACE
  adapters (forge API)             ██████████ 100%    GitHub/GitLab/Bitbucket
  cli (scanner)                    ██████████ 100%    Command-line interface
  data (storage)                   ██████████ 100%    ArangoDB + VeriSimDB
  fixer (auto-fix)                 ██████████ 100%    SHA pins, programmatic fixes

FORMAL VERIFICATION
  Idris2 ABI                       ██████████ 100%    Types, GraphQL, gRPC, REST + proofs
  Zig FFI Bridge                   ██████████ 100%    7 C ABI exports

CROSS-REPO INTEGRATION
  hypatia → verisimdb-data         ██████████ 100%    VQL + file I/O
  hypatia → gitbot-fleet           ████████░░  80%    Dispatch OK, fleet can't PR
  .git-private-farm → hypatia      ██░░░░░░░░  20%    repo-list done, HOOKSYNC_TOKEN missing

REMOVED (2026-03-06)
  Logtalk engine/ (28 .lgt files)  — absorbed into lib/rules/*.ex
  Ada TUI (102 files)              — never compiled
  Haskell registry (30 files)      — superseded by Elixir pipeline
  k8s/terraform/helm deploy (94)   — over-engineered for local tool
  static site, fuzz, clusterfuzz   — unused

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ████████░░  85%    Code complete, needs data + actuation
```

## Key Dependencies

```
verisimdb-data (canonical store)
       │
       ▼
PatternAnalyzer ──► TriangleRouter ──► Neural.Coordinator
       │                   │                    │
       │                   ▼                    │
       │            FleetDispatcher ◄───────────┘
       │                   │
       ▼                   ▼
OutcomeTracker      dispatch manifest
       │                   │
       ▼                   ▼
LearningScheduler   gitbot-fleet
       │            (detection only,
       ▼             no commit/PR)
Neural.Coordinator
(continuous learning)
```

## Critical Gaps

1. **Data seeding**: verisimdb-data dirs (patterns/recipes/outcomes) empty — neural networks idle
2. **Last-mile actuation**: gitbot-fleet detects problems but never commits or creates PRs
3. **HOOKSYNC_TOKEN**: .git-private-farm hookset propagation never tested
4. **Recipe coverage**: Need to expand from initial seed recipes

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
