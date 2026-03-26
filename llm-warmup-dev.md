<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- LLM warmup context — DEVELOPER level (<400 lines) -->
<!-- Feed this to an LLM before doing development work on Hypatia -->

# Hypatia — Developer Context

## Architecture

Hypatia is an Elixir OTP application with a Rust workspace, Idris2 ABI,
Zig FFI, and Logtalk rule engine.

### Elixir Pipeline (lib/)

Core OTP application with 8 GenServers supervised by `Hypatia.Application`.

| Module | Purpose |
|--------|---------|
| `pattern_analyzer.ex` | Full pipeline orchestrator: scan -> patterns -> triangle -> dispatch |
| `verisimdb_connector.ex` | VQL-powered data access with file I/O fallback |
| `pattern_registry.ex` | Deduplicates findings into canonical patterns (PA001-PA020) |
| `recipe_matcher.ex` | Fuzzy matching: fingerprinted IDs to clean recipe IDs |
| `triangle_router.ex` | Routes through Eliminate > Substitute > Control hierarchy |
| `fleet_dispatcher.ex` | Confidence-gated dispatch (file-based + HTTP, circuit breaker) |
| `dispatch_manifest.ex` | Writes JSONL manifests as bridge to bash execution |
| `outcome_tracker.ex` | Records fix outcomes, Bayesian confidence updating, re-scan verification |
| `recipe_generator.ex` | Auto-generates recipes for uncovered categories at 0.50 confidence |
| `scorecard_ingestor.ex` | Ingests 20 OpenSSF Scorecard checks as local scan patterns |
| `learning_scheduler.ex` | GenServer: polls outcomes every 5 min, drives feedback loop |
| `self_diagnostics.ex` | Health monitoring, circuit breaker, auto-recovery |
| `application.ex` | OTP Application supervisor for all GenServers |

### VQL Query Layer (lib/vql/)

| Module | Purpose |
|--------|---------|
| `client.ex` | GenServer: parser + query cache + execution routing |
| `file_executor.ex` | Executes VQL ASTs against verisimdb-data flat files |
| `query.ex` | High-level: fetch_scans, cross_repo_patterns, pipeline_health |

VQL is a custom query language for verisimdb-data. Currently local-only
(federation not yet implemented).

### Neural Subsystem (lib/neural/)

| Module | Type | Purpose |
|--------|------|---------|
| `graph_of_trust.ex` | PageRank | Trust over repos/bots/recipes |
| `mixture_of_experts.ex` | Sparse MoE | 7 expert domains |
| `liquid_state_machine.ex` | Reservoir | Temporal anomaly detection |
| `echo_state_network.ex` | Reservoir | Confidence trajectory forecasting |
| `radial_neural_network.ex` | RBF | Similarity, novelty, classification |
| `coordinator.ex` | GenServer | Orchestrates all 5 networks |
| `training_pipeline.ex` | — | ESN/RBF training from real outcomes |

Training reads `outcomes/*.jsonl` for ESN time series and
`patterns/registry.json` for RBF 8-D feature vectors.

### Safety Systems (lib/safety/)

| Module | Purpose |
|--------|---------|
| `rate_limiter.ex` | Per-bot (50/min), global (200/min), burst (10/5s) |
| `quarantine.ex` | 3 levels: soft/hard/permanent. Triggers on 5+ failures or >30% FP |
| `batch_rollback.ex` | Rollback entire dispatch batches with confidence revert |

### Rust Workspace

6 crates under the workspace:

| Crate | Purpose |
|-------|---------|
| `adapters` | Integration adapters |
| `cli` | CLI tools (`hyper` binary) |
| `data` | Data processing |
| `fixer` | Automated fix scripts |
| `integration` | Integration testing |
| `tools/cii-registrar` | CII Best Practices registration |

### Idris2 ABI (src/abi/)

5 modules with dependent type proofs:

| File | Purpose |
|------|---------|
| `Types.idr` | Core types with proofs |
| `GraphQL.idr` | Query/Mutation/Subscription with proofs |
| `GRPC.idr` | gRPC service definitions (scanner, dispatch, stream, health) |
| `REST.idr` | REST endpoint definitions (18 endpoints, 6 groups) |
| `FFI.idr` | GADT constructors for C ABI functions |
| `hypatia-abi.ipkg` | Package definition |

### Zig FFI (ffi/zig/)

7 exported C-ABI functions:

| Function | Purpose |
|----------|---------|
| `hypatia_health_check` | Health status of all components |
| `hypatia_scan_repo` | Trigger scan for repository |
| `hypatia_dispatch` | Dispatch finding to fleet |
| `hypatia_record_outcome` | Record fix outcome |
| `hypatia_force_learning_cycle` | Force learning cycle |
| `hypatia_get_confidence` | Get recipe confidence |
| `hypatia_dispatch_strategy` | Map confidence to dispatch strategy |

## Data Layer

verisimdb-data is a separate git-backed flat-file repo. Hypatia reads it via
`VerisimdbConnector` using VQL queries.

- Scan results: `scans/<repo>/<date>.json`
- Patterns: `patterns/registry.json`
- Outcomes: `outcomes/YYYY-MM.jsonl`
- Neural state: `data/verisimdb/neural-states/`

## Safety Triangle

Every finding is routed through:

1. **Eliminate** — Can the root cause be removed entirely?
2. **Substitute** — Can a safer alternative replace the current approach?
3. **Control** — Apply defensive measures (least preferred)

`TriangleRouter.route()` implements this hierarchy. Higher-level strategies
get higher confidence scores.

## Dispatch Pipeline

```
VerisimdbConnector.fetch_all_scans()
  → PatternRegistry.sync_from_scans()     (954 patterns)
  → TriangleRouter.route()                (Eliminate > Substitute > Control)
  → FleetDispatcher.dispatch_routed_action()
  → DispatchManifest.write()              (JSONL for dispatch-runner.sh)
```

Confidence tiers:
- >= 0.95: auto_execute via robot-repo-automaton
- 0.85-0.94: review via rhodibot (creates PR)
- < 0.85: report_only via sustainabot (advisory)

Bayesian Beta-distribution confidence: prior_strength=10, floor=0.10, cap=0.99.

## Build Commands

```bash
just build          # Rust workspace (release)
just build-all      # Rust + Elixir
just test           # Rust tests
just test-elixir    # Elixir tests
just fmt            # Format Rust + Elixir
just lint           # Clippy lints
just scan <path>    # Scan a repository
just cli <args>     # Run hypatia CLI
just compile-abi    # Compile Idris2 ABI
just build-ffi      # Build Zig FFI
just panic-scan     # panic-attacker analysis
just license-check  # Check for banned license headers
just check-scm      # Validate SCM file locations
just doctor         # Check prerequisites
just heal           # Install instructions
```

## Elixir Dependencies

From `mix.exs`:
- `jason` — JSON parsing
- `gen_stage` — Back-pressure for pipeline stages
- `phoenix` — Web framework (for future HTTP dashboard)
- `bandit` — HTTP server
- `plug` — HTTP middleware

## Known Gaps (for context)

1. VQL federation local-only (FileExecutor, not multi-store)
2. verisim-api not deployed (graph/vector/temporal via flat files only)
3. One-sided training data (99%+ success — needs failure data)
4. 310/600 auto-execute entries have null fix_script
5. Neural state persistence not yet writing to disk
6. Ada TUI not wired into OTP supervision

## Adding New Rules

Pattern recipes live in `patterns/`. To add a new pattern:

1. Add to `PatternRegistry` (unique PA code, e.g., PA021)
2. Add recipe in `recipes/` with fix script
3. Add triangle routing logic
4. Run `mix test` to verify
5. Outcomes will update confidence via Bayesian feedback

## Machine-Readable Metadata

All in `.machine_readable/6a2/`:
- `STATE.a2ml`, `META.a2ml`, `ECOSYSTEM.a2ml`
- `AGENTIC.a2ml`, `NEUROSYM.a2ml`, `PLAYBOOK.a2ml`

**NEVER** create these in the root directory.

## License

PMPL-1.0-or-later.
Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
Git author: 6759885+hyperpolymath@users.noreply.github.com
