<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- NEURAL-ARCHITECTURE.md — Hypatia neural subsystem architecture -->
<!-- Last updated: 2026-02-22 -->
<!-- Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->

# Hypatia — Neural Subsystem Architecture

## Overview

Hypatia's neural subsystem consists of **5 neural networks** orchestrated by a
central **Neural.Coordinator** GenServer in a **hub-and-spoke** topology. The
coordinator aggregates predictions from all 5 networks and provides a unified
intelligence layer for the safety triangle pipeline.

**Logtalk is NOT part of the neural architecture.** The Logtalk rule files
(`.lgt`) exist as standalone symbolic reasoning rules that are not loaded or
executed by the Elixir application. They are a separate, disconnected layer.

---

## Hub-and-Spoke Topology

```
                        ┌──────────────────────────────────────────┐
                        │       Neural.Coordinator (Hub)            │
                        │       GenServer in OTP supervision tree   │
                        │                                           │
                        │  Public API:                              │
                        │    analyze(finding)                       │
                        │    dispatch_recommendation(finding)       │
                        │    record_outcome(finding, outcome)       │
                        │    health_report()                        │
                        │    force_cycle()                          │
                        │                                           │
                        │  Aggregation weights:                     │
                        │    MoE: 60%  |  RBF: 25%  |  LSM: 15%   │
                        └───┬───┬───┬───┬───┬──────────────────────┘
                            │   │   │   │   │
              ┌─────────────┘   │   │   │   └─────────────┐
              │                 │   │   │                 │
    ┌─────────▼──────┐ ┌───────▼──┐│┌──▼──────────┐ ┌───▼──────────┐ ┌───▼──────────────┐
    │ Graph of Trust │ │ Mixture  │││ Liquid State │ │ Echo State   │ │ Radial Basis     │
    │ (PageRank)     │ │ of       │││ Machine      │ │ Network      │ │ Function (RBF)   │
    │                │ │ Experts  │││ (LSM)        │ │ (ESN)        │ │                  │
    │ Trust over     │ │ (MoE)   │││              │ │              │ │ Similarity +     │
    │ repos/bots/    │ │ 7 expert│││ Temporal     │ │ Confidence   │ │ novelty          │
    │ recipes        │ │ domains │││ anomaly      │ │ trajectory   │ │ detection        │
    │                │ │ + gate  │││ detection    │ │ forecasting  │ │                  │
    └────────────────┘ └─────────┘│└──────────────┘ └──────────────┘ └──────────────────┘
                                  │
                        (spokes do NOT communicate
                         with each other directly)
```

### Key Properties

- **Hub-and-spoke, NOT mesh**: The 5 networks never talk to each other.
  All communication flows through the coordinator.
- **Weighted aggregation**: MoE (60%), RBF (25%), LSM (15%). The Graph of
  Trust and ESN inform routing/trajectory but don't contribute to the
  aggregated confidence score directly.
- **Novelty gating**: If RBF detects a novel finding (distance from known
  patterns exceeds threshold), the coordinator forces `:report_only`
  strategy regardless of confidence score.
- **Conservative adjustment**: When wired into the pipeline, the neural
  aggregated confidence acts as a lower bound on recipe confidence —
  it can demote actions to a lower tier but never promote them.

---

## The 5 Neural Networks

### 1. Graph of Trust (PageRank)

**File:** `lib/neural/graph_of_trust.ex`
**Purpose:** Trust-weighted routing over repos, bots, and recipes.

Builds a directed graph where nodes are repos/bots/recipes and edges
represent fix outcomes (success = +1.0 weight, failure = -0.5, false
positive = -1.0). Uses iterative PageRank with damping factor 0.85
and convergence threshold 0.001.

**Outputs:**
- `trust_score(entity_id)` — trust for any entity (default 0.5)
- `trusted_bots()` — ranked list of bots by trust
- `trusted_recipes()` — ranked list of recipes by trust
- `repos_needing_attention()` — repos with trust < 0.4

**Data source:** `verisimdb-data/outcomes/*.jsonl` + `verisimdb-data/recipes/*.json`

### 2. Mixture of Experts (MoE)

**File:** `lib/neural/mixture_of_experts.ex`
**Purpose:** Domain-specific confidence estimation.

7 expert domains, each specialized in a category of findings. A gating
network routes each finding to the most relevant expert(s) and combines
their predictions.

**Outputs:**
- `{confidence, experts}` — predicted confidence + which experts contributed
- `expert_utilization()` — usage statistics per expert

### 3. Liquid State Machine (LSM)

**File:** `lib/neural/liquid_state_machine.ex`
**Purpose:** Temporal anomaly detection in event streams.

Processes findings as a time series. Maintains a reservoir of recent events
and detects unusual patterns (sudden spikes in findings, category shifts,
confidence drift).

**Outputs:**
- `{score, status}` — temporal health score + `:normal` / `:warning` / `:critical`
- `detect_anomalies()` — list of detected anomalies

### 4. Echo State Network (ESN)

**File:** `lib/neural/echo_state_network.ex`
**Purpose:** Confidence trajectory forecasting and drift detection.

Predicts where recipe confidence is heading (improving, stable, degrading).
Training data source: `verisimdb-data/outcomes/*.jsonl` (confidence time series per recipe).

**Outputs:**
- `{predicted_next, updated_esn}` — next predicted confidence value
- `detect_drift()` — confidence drift alerts

**Training:** Via `TrainingPipeline.train_esn()`, called by `force_cycle()`.

### 5. Radial Basis Function Network (RBF)

**File:** `lib/neural/radial_neural_network.ex`
**Purpose:** Finding similarity and novelty detection.

Converts findings to 8-dimensional feature vectors and classifies them
using radial basis functions.
Training data source: `verisimdb-data/patterns/registry.json` (8-D feature vectors).

**Outputs:**
- `{output, confidence}` — classification result + confidence
- `detect_novelty(feature_vec)` — `:known` or `{:novel, distance}`

**Training:** Via `TrainingPipeline.train_rbf()`, called by `force_cycle()`.

---

## Pipeline Integration

### Before 2026-02-22 (Dead Code)

The neural coordinator existed in the OTP supervision tree but was never
called by the main pipeline. All dispatch decisions used hardcoded
thresholds (0.95 for auto_execute, 0.85 for review) from recipe
confidence scores only.

```
PatternAnalyzer.analyze_all_scans()
  → TriangleRouter.route()           (hardcoded thresholds)
  → FleetDispatcher.dispatch()       (no neural input)
  → OutcomeTracker.record()          (no neural feedback)

Neural.Coordinator                   (sitting in OTP tree, never called)
```

### After 2026-02-22 (Wired In)

The coordinator is now consulted during the pipeline via
`PatternAnalyzer.enhance_with_neural/1`:

```
PatternAnalyzer.analyze_all_scans()
  → TriangleRouter.route()           (rule-based routing)
  → enhance_with_neural()            ← NEW: consults coordinator
  │   → Coordinator.dispatch_recommendation(finding)
  │       → MoE predicts confidence
  │       → RBF checks novelty
  │       → Graph of Trust ranks bots/recipes
  │       → LSM checks for temporal anomalies
  │       → ESN predicts confidence trajectory
  │   → Novel findings → demoted to :control (human review)
  │   → Non-novel: min(recipe_conf, neural_conf) used (conservative)
  → FleetDispatcher.dispatch()       (neural-informed decisions)
  → OutcomeTracker.record()
  │   → Coordinator.record_outcome() ← NEW: feeds learning loop
  │       → Updates MoE, LSM, ESN
```

### Graceful Degradation

If the neural coordinator is unavailable (crashed, not started, empty data),
the pipeline falls back to the original behavior — recipe confidence with
hardcoded thresholds. The `try/catch :exit` in `neural_recommendation/1`
handles this transparently.

---

## Training Pipeline

```
TrainingPipeline.run_full_training()
  │
  ├── train_esn()
  │     reads: verisimdb-data/outcomes/*.jsonl
  │     builds: confidence time series per recipe
  │     trains: ESN reservoir weights
  │
  └── train_rbf()
        reads: verisimdb-data/patterns/registry.json
        builds: 8-dimensional feature vectors (965 patterns)
        trains: RBF centers + widths + output weights
        MSE: 0.047

Triggered by: Coordinator.force_cycle()
```

### Training Status

Networks initialize with default weights and learn from real data as it
arrives. The `LearningScheduler` polls `verisimdb-data/outcomes/` every
5 minutes and feeds outcomes to the coordinator via `record_outcome/2`.

Training is seeded from gitbot-fleet's `fix-outcomes.jsonl` (6,000+ records)
and hypatia's `auto-fix-formulaic.sh` scan results.

**Note:** The Logtalk symbolic layer (`.lgt` files) was absorbed into
Elixir modules in `lib/rules/` on 2026-03-06. The Logtalk files have
been removed from the repository.

---

## JSON Ruleset Layer (Active)

The active rule engine uses JSON rulesets in `registry/rulesets/`:

| Ruleset | Rules | Purpose |
|---------|-------|---------|
| `rsr-compliance.json` | 15 | RSR language + file policy |
| `security-baseline.json` | 8 | OpenSSF security practices |
| `workflow-hygiene.json` | 10 | GitHub Actions standards |
| `release-readiness.json` | 10 | Release gate checks |
| `rust-quality.json` | varies | Rust-specific quality |

These are the rules that actually drive the pipeline via
`PatternRegistry.sync_from_scans()` and `RecipeMatcher`.

---

## Cross-Repo Integration

```
                    .git-private-farm
                    (admin registry)
                          │
                    NOT CONNECTED
                          │
┌─────────────────────────▼──────────────────────────────┐
│                      HYPATIA                            │
│                                                         │
│  verisimdb-data ──► PatternAnalyzer ──► TriangleRouter │
│       │                    │                    │       │
│       │              Neural.Coordinator         │       │
│       │              (hub-and-spoke)             │       │
│       │                    │                    │       │
│       │              FleetDispatcher ───────────┘       │
│       │                    │                            │
│       └── outcomes ◄───────┘                            │
└────────────────────────────┬───────────────────────────┘
                             │
                    dispatch manifest (JSONL)
                             │
┌────────────────────────────▼───────────────────────────┐
│                    GITBOT-FLEET                         │
│                                                         │
│  dispatch-runner.sh ──► fix-*.sh (7 scripts)           │
│       │                    │                            │
│       │              FILES MODIFIED LOCALLY              │
│       │                    │                            │
│       │              git commit ❌ (NEVER)              │
│       │              git push   ❌ (NEVER)              │
│       │              create PR  ❌ (NEVER)              │
│       │                                                 │
│  robot-repo-automaton (5% complete for git workflow)    │
└─────────────────────────────────────────────────────────┘

                    stateful-artefacts
                    (cross-AI comms)
                          │
                    NOT CONNECTED
                          │
                    Gnosis engine ──► rendered docs
```

### Integration Status

| Link | Status | Notes |
|------|--------|-------|
| hypatia → verisimdb-data | Working | VQL + file I/O |
| hypatia → gitbot-fleet | Working | Dispatch manifest JSONL |
| hypatia neural → pipeline | **Fixed 2026-02-22** | Was dead code |
| gitbot-fleet → repos | **BROKEN** | Never commits or PRs |
| .git-private-farm → hypatia | Not connected | No enrollment |
| stateful-artefacts → hypatia | Not connected | No integration |
| hypatia → ArangoDB | Degraded | File-only mode |

---

## File Inventory

### Elixir Pipeline (lib/)

| Module | Purpose |
|--------|---------|
| `application.ex` | OTP supervisor |
| `pattern_analyzer.ex` | Full pipeline orchestrator |
| `verisimdb_connector.ex` | VQL-powered data access |
| `pattern_registry.ex` | Deduplicates to canonical patterns |
| `recipe_matcher.ex` | Fuzzy matching with language inference |
| `triangle_router.ex` | Safety triangle: eliminate > substitute > control |
| `fleet_dispatcher.ex` | Bot dispatch (file + HTTP) |
| `dispatch_manifest.ex` | JSONL bridge to bash execution |
| `outcome_tracker.ex` | Records outcomes, updates confidence |
| `learning_scheduler.ex` | GenServer polling every 5 min |
| `self_diagnostics.ex` | Health monitoring, circuit breaker |

### Rules Engine (lib/rules/) — absorbed from Logtalk 2026-03-06

| Module | Purpose |
|--------|---------|
| `rules.ex` | Facade: scan_file/3, scan_workflow/1 |
| `security_errors.ex` | SHA pins, secret patterns, CWE mappings |
| `cicd_rules.ex` | Commit blocking, waste detection, error catalog |
| `code_safety.ex` | Per-language dangerous patterns (Rust, Idris2, Haskell, etc.) |
| `migration_rules.ex` | ReScript API migration, merge conflict resolution |
| `learning.ex` | GenServer: fix outcome tracking, confidence scoring |
| `forge_adapters.ex` | Forge operations with input validation |

### Neural Subsystem (lib/neural/)

| Module | Purpose |
|--------|---------|
| `coordinator.ex` | Hub: orchestrates all 5 networks |
| `graph_of_trust.ex` | PageRank trust scoring |
| `mixture_of_experts.ex` | Domain-specific confidence (7 expert domains) |
| `liquid_state_machine.ex` | Temporal anomaly detection |
| `echo_state_network.ex` | Confidence trajectory forecasting |
| `radial_neural_network.ex` | Similarity + novelty detection |
| `training_pipeline.ex` | ESN + RBF training from verisimdb-data |
| `persistence.ex` | Save/load neural state |

### Safety Systems (lib/safety/)

| Module | Purpose |
|--------|---------|
| `rate_limiter.ex` | 50/min/bot, 200/min global, 10/5s burst |
| `quarantine.ex` | Auto on 5+ failures or >30% FP rate |
| `batch_rollback.ex` | Rollback dispatch batches |

### Rust Workspace (adapters/, cli/, data/, fixer/)

| Crate | Purpose |
|-------|---------|
| `adapters` | GitHub/GitLab/Bitbucket forge API adapters |
| `cli` | Command-line scanner interface |
| `data` | ArangoDB + VeriSimDB data layer |
| `fixer` | Programmatic CI/CD auto-fixer with SHA pin database |

---

## Appendix: OTP Supervision Tree

```
Hypatia.Supervisor (:one_for_one)
├── Layer 0: Hypatia.VQL.Client
├── Layer 1: Hypatia.Data.ArangoDB
├── Layer 2: Hypatia.Safety.RateLimiter
├── Layer 3: Hypatia.Safety.Quarantine
├── Layer 4: Hypatia.LearningScheduler
├── Layer 5: Hypatia.SelfDiagnostics
└── Layer 6: Hypatia.Neural.Coordinator
                ├── GraphOfTrust
                ├── MixtureOfExperts
                ├── LiquidStateMachine
                ├── EchoStateNetwork
                └── RadialNeuralNetwork
```

All GenServers use `:one_for_one` strategy — if one crashes, only that
process is restarted. The Neural.Coordinator initializes all 5 networks
in its `init/1` callback. If outcomes data is empty, it now handles this
gracefully (empty data guard added 2026-02-22).
