<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- NEURAL-ARCHITECTURE.md — Hypatia neural subsystem architecture -->
<!-- Last updated: 2026-02-22 -->
<!-- Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> -->

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

Trained on real outcome data (2,372 data points from `verisimdb-data/outcomes/`).
Predicts where recipe confidence is heading (improving, stable, degrading).

**Outputs:**
- `{predicted_next, updated_esn}` — next predicted confidence value
- `detect_drift()` — confidence drift alerts

**Training:** Via `TrainingPipeline.train_esn()`, called by `force_cycle()`.

### 5. Radial Basis Function Network (RBF)

**File:** `lib/neural/radial_neural_network.ex`
**Purpose:** Finding similarity and novelty detection.

Converts findings to 8-dimensional feature vectors and classifies them
using radial basis functions. Trained on 965 pattern vectors from
`verisimdb-data/patterns/registry.json` (MSE = 0.047).

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

### Known Training Gap

All 3,588+ training outcomes are "success" (100%). The networks have
never seen failure or false-positive data. This means:

- ESN confidence trajectories are monotonically increasing
- RBF has no decision boundary between success and failure
- MoE cannot distinguish high-risk from low-risk domains
- Confidence estimates are inflated across the board

**To fix:** Intentionally dispatch lower-confidence findings to generate
failure data, or inject synthetic failure cases for balanced training.

---

## Logtalk Symbolic Layer (Disconnected)

The Logtalk `.lgt` files exist as standalone symbolic reasoning rules:

| File | Purpose | Status |
|------|---------|--------|
| `security_errors.lgt` | Error catalog (10 types) | Reference data |
| `cicd_rules.lgt` | 200+ declarative rules | Many stubs |
| `pattern_matching.lgt` | Pattern detection | Complete |
| `learning.lgt` | Learning rule distillation | Complete |
| `error_catalog.lgt` | Error type definitions | Mostly complete |
| `prevention_hooks.lgt` | Pre-commit prevention | Complete |
| `rule_distiller.lgt` | Rule extraction | Complete |
| `forge_adapters.lgt` | Forge-specific rules | Complete |
| `error_instances.lgt` | Error instances | **STUB** |
| `loader.lgt` | Rule loader | **STUB** |

**These are NOT loaded by the Elixir application.** There is no integration
bridge between Logtalk rules and the Elixir pipeline. The rules exist as
a separate knowledge base that could be integrated in the future via:

1. A Logtalk-to-JSON rule compiler
2. An Elixir NIF calling SWI-Prolog
3. A pre-processing step that generates Elixir modules from Logtalk rules

### Stub Predicates (Blocking Rule Execution)

These predicates are called in rules but never defined:

- `commit_adds_file/2`
- `commit_modifies_workflow/1`
- `file_has_spdx_header/1`
- `workflow_has_unpinned_action/1`
- `repo_is_spec_only/1`
- `repo_is_public/1`

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

| Module | Lines | Purpose |
|--------|-------|---------|
| `application.ex` | 38 | OTP supervisor (7 GenServers) |
| `pattern_analyzer.ex` | ~190 | Full pipeline orchestrator |
| `verisimdb_connector.ex` | ~200 | VQL-powered data access |
| `pattern_registry.ex` | ~150 | Deduplicates to canonical patterns |
| `recipe_matcher.ex` | ~180 | Fuzzy matching with language inference |
| `triangle_router.ex` | 179 | Safety triangle: eliminate > substitute > control |
| `fleet_dispatcher.ex` | ~450 | Bot dispatch (file + HTTP) |
| `dispatch_manifest.ex` | ~100 | JSONL bridge to bash execution |
| `outcome_tracker.ex` | ~120 | Records outcomes, updates confidence |
| `learning_scheduler.ex` | ~80 | GenServer polling every 5 min |
| `self_diagnostics.ex` | ~100 | Health monitoring, circuit breaker |

### Neural Subsystem (lib/neural/)

| Module | Lines | Purpose |
|--------|-------|---------|
| `coordinator.ex` | 253 | Hub: orchestrates all 5 networks |
| `graph_of_trust.ex` | 227 | PageRank trust scoring |
| `mixture_of_experts.ex` | ~250 | Domain-specific confidence |
| `liquid_state_machine.ex` | ~200 | Temporal anomaly detection |
| `echo_state_network.ex` | ~200 | Confidence trajectory forecasting |
| `radial_neural_network.ex` | ~250 | Similarity + novelty detection |
| `training_pipeline.ex` | ~150 | ESN + RBF training |

### Safety Systems (lib/safety/)

| Module | Lines | Purpose |
|--------|-------|---------|
| `rate_limiter.ex` | ~80 | 50/min/bot, 200/min global, 10/5s burst |
| `quarantine.ex` | ~100 | Auto on 5+ failures or >30% FP rate |
| `batch_rollback.ex` | ~80 | Rollback dispatch batches |

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
