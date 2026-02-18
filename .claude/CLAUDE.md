# CLAUDE.md - Hypatia AI Assistant Instructions

## Project Overview

Hypatia is the neurosymbolic CI/CD intelligence layer for the hyperpolymath ecosystem. It coordinates the gitbot-fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot) via a safety triangle pipeline, with 5 neural networks for intelligent dispatch, ArangoDB for graph queries alongside verisimdb-data (git-backed canonical store), and Logtalk rules for pattern detection.

## Architecture

```
Hypatia
├── Elixir pipeline          # 8 core modules (pattern analysis, dispatch, learning)
├── Neural subsystem          # 5 networks + coordinator GenServer
│   ├── Graph of Trust        # PageRank trust over repos/bots/recipes
│   ├── Mixture of Experts    # Domain-specific confidence (7 expert domains)
│   ├── Liquid State Machine  # Temporal anomaly detection
│   ├── Echo State Network    # Confidence trajectory forecasting
│   └── Radial Neural Network # Finding similarity + novelty detection
├── VQL query layer            # Built-in parser, file executor, query cache
├── Data layer                 # ArangoDB (federated) + verisimdb-data (canonical)
├── Safety systems             # Rate limiter, quarantine, batch rollback
├── OTP Application           # 7 GenServers: VQL, ArangoDB, RateLimiter, Quarantine, Learning, Diag, Neural
├── Logtalk rule engine       # Error catalog, pattern detection rules
├── Idris2 ABI               # Types, GraphQL, gRPC, REST with dependent type proofs
├── Zig FFI                   # C ABI bridge (7 exported functions)
├── Rust workspace            # adapters, cli, data, fixer, integration
├── Safety triangle           # Eliminate > Substitute > Control
├── Fleet dispatcher          # File-based + HTTP dispatch with circuit breaker
└── Integration connectors    # verisimdb-data, panic-attack, gitbot-fleet
```

## Key Commands

```bash
mix deps.get    # Install Elixir deps
mix test        # Run tests
mix format      # Format Elixir code
cargo build     # Build Rust workspace
cargo test      # Test Rust workspace
```

## Machine-Readable Artefacts

Files in `.machine_readable/` contain structured project metadata:

- `STATE.scm` - Current project state and progress
- `META.scm` - Architecture decisions and development practices
- `ECOSYSTEM.scm` - Position in the ecosystem and related projects
- `AGENTIC.scm` - AI agent interaction patterns
- `NEUROSYM.scm` - Neurosymbolic integration config
- `PLAYBOOK.scm` - Operational runbook

## Safety Triangle Pipeline (OPERATIONAL)

### Data Flow

```
panic-attack assail (scan repos)
        | JSON results
verisimdb-data repo (git-backed flat-file store, 292 repos scanned)
        | read scan results
Elixir pipeline:
  VerisimdbConnector.fetch_all_scans()
        |
  PatternRegistry.sync_from_scans()     -- 954 canonical patterns
        |
  TriangleRouter.route()                -- Eliminate > Substitute > Control
        |
  FleetDispatcher.dispatch_routed_action()
        |
  DispatchManifest.write()              -- JSONL for execution layer
        |
dispatch-runner.sh (gitbot-fleet)
  ├── auto_execute (>=0.95 confidence): robot-repo-automaton
  ├── review (0.85-0.94): rhodibot creates PR
  └── report_only (<0.85): sustainabot advisory
        |
OutcomeTracker.record_outcome()         -- Feedback loop
```

### Core Elixir Modules (lib/)

| Module | Purpose |
|--------|---------|
| `pattern_analyzer.ex` | Full pipeline orchestrator: scan -> patterns -> triangle -> dispatch |
| `verisimdb_connector.ex` | VQL-powered data access with file I/O fallback |
| `pattern_registry.ex` | Deduplicates findings into canonical patterns (PA001-PA020) |
| `recipe_matcher.ex` | Fuzzy matching: fingerprinted IDs to clean recipe IDs |
| `triangle_router.ex` | Routes through Eliminate > Substitute > Control hierarchy |
| `fleet_dispatcher.ex` | Confidence-gated dispatch (file-based + HTTP, circuit breaker) |
| `dispatch_manifest.ex` | Writes JSONL manifests as bridge to bash execution |
| `outcome_tracker.ex` | Records fix outcomes, updates recipe confidence |
| `learning_scheduler.ex` | GenServer: polls outcomes every 5 min, drives feedback loop |
| `self_diagnostics.ex` | Health monitoring, circuit breaker, auto-recovery |
| `application.ex` | OTP Application supervisor for all GenServers |

### VQL Query Layer (lib/vql/)

| Module | Purpose |
|--------|---------|
| `client.ex` | VQL Client GenServer: parser + query cache + execution routing |
| `file_executor.ex` | Executes VQL ASTs against verisimdb-data flat files |
| `query.ex` | High-level query functions: fetch_scans, cross_repo_patterns, pipeline_health |

### Neural Network Modules (lib/neural/)

| Module | Type | Purpose |
|--------|------|---------|
| `graph_of_trust.ex` | PageRank | Trust-weighted routing over repos/bots/recipes |
| `mixture_of_experts.ex` | Sparse MoE | Domain-specific confidence (7 expert domains) |
| `liquid_state_machine.ex` | Reservoir | Temporal anomaly detection in event streams |
| `echo_state_network.ex` | Reservoir | Confidence trajectory forecasting + drift detection |
| `radial_neural_network.ex` | RBF | Finding similarity, novelty detection, classification |
| `coordinator.ex` | GenServer | Orchestrates all 5 networks, aggregates predictions |

### Neural Training (lib/neural/)

| Module | Purpose |
|--------|---------|
| `training_pipeline.ex` | ESN/RBF training from real verisimdb-data outcomes + pattern vectors |

Training pipeline reads outcomes/*.jsonl for ESN (confidence time series) and patterns/registry.json for RBF (8-D feature vectors). Coordinator's `:force_cycle` triggers training automatically.

### Idris2 ABI (src/abi/)

| File | Purpose |
|------|---------|
| `Types.idr` | Core types with dependent type proofs |
| `GraphQL.idr` | Query/Mutation/Subscription operations with proofs |
| `GRPC.idr` | gRPC service definitions (scanner, dispatch, stream, health) |
| `REST.idr` | REST endpoint definitions (18 endpoints, 6 groups) |
| `FFI.idr` | GADT constructors for all C ABI functions + ffiReturnsApiResponse proof |

**Build system:** `src/abi/hypatia-abi.ipkg` (compile), `verify/hypatia-verify.ipkg` (proofs), `pack.toml` (Pack package manager)

### Zig FFI (ffi/zig/src/)

| Function | Purpose |
|----------|---------|
| `hypatia_health_check` | Health status of all components |
| `hypatia_scan_repo` | Trigger scan for repository |
| `hypatia_dispatch` | Dispatch finding to fleet |
| `hypatia_record_outcome` | Record fix outcome |
| `hypatia_force_learning_cycle` | Force learning cycle |
| `hypatia_get_confidence` | Get recipe confidence |
| `hypatia_dispatch_strategy` | Map confidence to dispatch strategy |

### Data Layer (lib/data/)

| Module | Purpose |
|--------|---------|
| `arangodb.ex` | Elixir ArangoDB client, auto-sync from verisimdb-data every 10 min |
| `models.ex` | 12 document models + 3 edge models for extended E-R schema |

**Federated architecture:** verisimdb-data (git) is canonical; ArangoDB provides graph queries, trust traversal, neural state, confidence history. Auto-sync keeps them in harmony.

**Collections:** repos, findings, patterns, bots, recipes, outcomes, contributors, confidence_history, anomalies, dispatch_batches, neural_states, sessions, rulesets, learning_data + 9 edge collections.

### Safety Systems (lib/safety/)

| Module | Purpose |
|--------|---------|
| `rate_limiter.ex` | Per-bot (50/min), global (200/min), burst (10/5s) dispatch limits |
| `quarantine.ex` | Auto-quarantine on 5+ failures or >30% FP rate; 3 levels (soft/hard/permanent) |
| `batch_rollback.ex` | Rollback entire dispatch batches with confidence revert |

### Metrics (as of 2026-02-13)

- 385 auto_execute fixes dispatched (0 failures)
- 86.3% weak point reduction: 3260 -> 447
- 404 outcomes recorded (100% success rate)
- 6 recipes at 0.99 confidence
- 22 fix recipes total (298 repos scanned)
- 954 canonical patterns across 298 repos
- ESN trained on 2,372 real confidence data points
- 5 neural networks + coordinator in OTP supervision
- 14 document + 9 edge ArangoDB collections defined
- 3 safety systems: rate limiter, quarantine, batch rollback
- VQL integrated: built-in parser, file executor, query cache, cross-repo analytics

### Remaining Work (M7: Production Operations)

**Critical (this week):**
- ~~Integrate VQL into verisimdb_connector.ex~~ DONE — VQL Client + FileExecutor + Query module
- Deploy ArangoDB + Dragonfly in production
- Create PAT with repo scope for automated cross-repo dispatch
- Generate summaries for 184 NULL-summary repos in verisimdb-data

**Important (this month):**
- Deploy verisim-api server (enables native graph/vector/temporal modalities)
- Implement VQL federation executor (currently local-only)
- ~~Fix RBF training (registry.json path needs investigation)~~ DONE — 965 vectors, MSE=0.047
- Historical trend tracking across scan cycles
- ~~VQL test files for client.ex, file_executor.ex, query.ex~~ DONE — 223 tests

**Planned:**
- GraphQL API as live HTTP endpoint
- SARIF output for IDE integration
- Chapel NIFs for compute-heavy neural operations
- Cross-organization federation with VQL drift policies

### Known Gaps

1. **VQL federation local-only:** FileExecutor handles FEDERATION queries against local files, not multi-store
2. **verisim-api not deployed:** VeriSimDB Rust core not running — graph/vector/temporal modalities via flat files only
3. **One-sided training data:** All 3,588 outcomes are "success" — ESN trained but needs failure data for balanced learning
4. **RBF untrained:** patterns/registry.json path needs investigation
5. **Recipe coverage 2.3%:** 22 recipes for 954 patterns — 932 patterns have no automated fix
6. **ArangoDB transitional:** Fills graph gap until verisim-api deployed
7. **Containerfiles:** SWI-Prolog and Haskell still use non-Chainguard base images (no Chainguard equivalents)
8. **Ada TUI not integrated:** Compiles but not wired into Elixir supervision tree

## Code Style

- Elixir: `mix format`, SPDX headers on all files
- Rust: `rustfmt`, `clippy`
- Logtalk: Follow coding guidelines
- Shell: `ShellCheck`, POSIX-compatible
- SPDX-License-Identifier: PMPL-1.0-or-later

## Security Requirements

- No MD5/SHA1 for security (use SHA256+)
- HTTPS only (no HTTP URLs)
- No hardcoded secrets
- SHA-pinned dependencies
- SPDX license headers on all files
- Rate limiting on all dispatch operations
- Bot quarantine on repeated failures
- Batch rollback capability for auto_execute tier
- Novelty gating: unknown findings forced to report_only
