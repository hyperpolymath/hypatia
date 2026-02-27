<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-22 -->

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
  Pattern Registry                 ██████████ 100%    954 canonical patterns
  Recipe Matcher                   ██████████ 100%    Fuzzy matching + language inference
  Triangle Router                  ██████████ 100%    Eliminate > Substitute > Control
  Fleet Dispatcher                 █████████░  90%    File dispatch ✓, HTTP graceful
  Dispatch Manifest                ██████████ 100%    JSONL bridge to bash execution
  Pattern Analyzer                 ██████████ 100%    Full pipeline orchestrator

NEURAL SUBSYSTEM (hub-and-spoke)
  Neural Coordinator               ████████░░  80%    Wired into pipeline 2026-02-22
  Graph of Trust (PageRank)        ████████░░  80%    Boot crash fixed 2026-02-22
  Mixture of Experts               ██████████ 100%    7 expert domains + gating
  Liquid State Machine             ██████████ 100%    Temporal anomaly detection
  Echo State Network               ████████░░  80%    Trained (2372 pts), one-sided data
  Radial Basis Function            ████████░░  80%    Trained (965 vecs), one-sided data
  Training Pipeline                ██████████ 100%    ESN + RBF from real data

LEARNING & SAFETY
  Outcome Tracker                  ██████████ 100%    Records outcomes, updates confidence
  Learning Scheduler               ████████░░  80%    Polls every 5 min, one-sided data
  Rate Limiter                     ██████████ 100%    50/min/bot, 200/min global
  Quarantine                       ██████████ 100%    Auto on 5+ failures or >30% FP
  Batch Rollback                   ██████████ 100%    Confidence revert capability

RULE ENGINE
  JSON Rulesets (5 files)          ██████████ 100%    15 RSR + 8 security + 10 workflow
  Logtalk Rules (10 files)         ██████░░░░  60%    Many stubs, NOT loaded by Elixir
  Recipe Coverage                  ░░░░░░░░░░   2%    22 recipes / 954 patterns

DATA LAYER
  VeriSimDB Connector              ██████████ 100%    VQL + file I/O fallback
  ArangoDB Client                  █████░░░░░  50%    Exists, not deployed (file-only)
  VQL Federation                   ███░░░░░░░  30%    Local files only, no multi-store

FORMAL VERIFICATION
  Idris2 ABI                       ██████████ 100%    Types, GraphQL, gRPC, REST + proofs
  Zig FFI Bridge                   ██████████ 100%    7 C ABI exports
  FFI.idr Proofs                   ██████████ 100%    GADT + ffiReturnsApiResponse

INFRASTRUCTURE
  OTP Supervision Tree             ██████████ 100%    7 GenServers, :one_for_one
  Containerfile (Chainguard)       ██████████ 100%    Multi-stage, non-root
  License Compliance               ██████████ 100%    All PMPL-1.0-or-later (fixed 2026-02-22)
  Tests (Elixir)                   ████████░░  80%    11 test files, no neural tests

CROSS-REPO INTEGRATION
  hypatia → verisimdb-data         ██████████ 100%    Working (VQL + files)
  hypatia → gitbot-fleet           ████████░░  80%    Dispatch works, fleet can't PR
  .git-private-farm → hypatia      ░░░░░░░░░░   0%    Not enrolled
  stateful-artefacts → hypatia     ░░░░░░░░░░   0%    Not connected

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ███████░░░  75%    Neural wired, fleet actuation missing
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

1. **Recipe coverage**: 22/954 patterns (2.3%) — 932 patterns have no automated fix
2. **One-sided training**: All 3,588 outcomes are "success" — no failure data
3. **gitbot-fleet**: Detects problems, never commits or creates PRs
4. **Logtalk disconnected**: Rules exist but not loaded by Elixir app
5. **ArangoDB not deployed**: Running in file-only degraded mode
6. **Cross-repo integration**: .git-private-farm and stateful-artefacts not connected

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
