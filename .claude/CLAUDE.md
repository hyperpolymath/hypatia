# CLAUDE.md - Hypatia AI Assistant Instructions

## Project Overview

Hypatia is the neurosymbolic CI/CD intelligence layer for the hyperpolymath ecosystem. It coordinates the gitbot-fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot) via a safety triangle pipeline, with 5 neural networks for intelligent dispatch, verisim-data (git-backed canonical flat-file store) with VCL queries, Bayesian confidence updating, and Elixir rules for pattern detection.

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
├── VCL query layer            # Built-in parser, file executor, query cache
├── Data layer                 # verisim-data (canonical flat-file store)
├── Safety systems             # Rate limiter, quarantine, batch rollback
├── OTP Application           # 8 GenServers: VCL, RateLimiter, Quarantine, Learning, Diag, Neural, Kin
├── Elixir rules engine       # Error catalog, pattern detection rules (lib/rules/)
├── Idris2 ABI               # Types, GraphQL, gRPC, REST with dependent type proofs
├── Zig FFI                   # C ABI bridge (7 exported functions)
├── Rust workspace            # adapters, cli, data, fixer, integration
├── Safety triangle           # Eliminate > Substitute > Control
├── Fleet dispatcher          # File-based + HTTP dispatch with circuit breaker
└── Integration connectors    # verisim-data, panic-attack, gitbot-fleet
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

- `.machine_readable/6a2/STATE.a2ml` - Current project state and progress
- `.machine_readable/6a2/META.a2ml` - Architecture decisions and development practices
- `.machine_readable/6a2/ECOSYSTEM.a2ml` - Position in the ecosystem and related projects
- `.machine_readable/6a2/AGENTIC.a2ml` - AI agent interaction patterns
- `.machine_readable/6a2/NEUROSYM.a2ml` - Neurosymbolic integration config
- `.machine_readable/6a2/PLAYBOOK.a2ml` - Operational runbook

## Safety Triangle Pipeline (OPERATIONAL)

### Data Flow

```
panic-attack assail (scan repos)
        | JSON results
verisim-data repo (git-backed flat-file store, 292 repos scanned)
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
| `verisimdb_connector.ex` | VCL-powered data access with file I/O fallback |
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

### VCL Query Layer (lib/vcl/)

| Module | Purpose |
|--------|---------|
| `client.ex` | VCL Client GenServer: parser + query cache + execution routing |
| `file_executor.ex` | Executes VCL ASTs against verisim-data flat files |
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
| `training_pipeline.ex` | ESN/RBF training from real verisim-data outcomes + pattern vectors |

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

### Data Layer

verisim-data (git-backed flat files) is the canonical data store. VCL queries execute against it directly via FileExecutor. Neural state persists to `data/verisim/neural-states/`. Outcomes append to `outcomes/YYYY-MM.jsonl`.

### Safety Systems (lib/safety/)

| Module | Purpose |
|--------|---------|
| `rate_limiter.ex` | Per-bot (50/min), global (200/min), burst (10/5s) dispatch limits |
| `quarantine.ex` | Auto-quarantine on 5+ failures or >30% FP rate; 3 levels (soft/hard/permanent) |
| `batch_rollback.ex` | Rollback entire dispatch batches with confidence revert |

### Metrics (as of 2026-04-22)

- 302 repos scanned, 3385 weak points across ecosystem
- 1635 dispatched actions (600 auto-execute, 667 review, 368 report)
- 16671 outcomes recorded (99% success rate, Bayesian confidence updating)
- 46 recipes (0 uncovered categories), 20 OpenSSF Scorecard recipes, 5 RSR compliance rules
- 5 neural networks + coordinator in OTP supervision
- Bayesian Beta-distribution confidence (prior_strength=10, floor=0.10, cap=0.99)
- Re-scan verification via panic-attacker (confirms fix removed weak point)
- PanLL data bridge: generates real-time JSON for dashboard panels
- 3 safety systems: rate limiter, quarantine, batch rollback
- VCL integrated: built-in parser, file executor, query cache, cross-repo analytics

### Remaining Work (M7: Production Operations)

**Critical:**
- Create PAT with repo scope for automated cross-repo dispatch
- Write real fix scripts for the 310 null-fix-script dispatch entries
- Push committed fixes to remotes across repos

**Important:**
- Deploy verisim-api server (enables native graph/vector/temporal modalities)
- 5 new RSR compliance rules cover structural compliance (banned languages, SCM locations, required files, Containerfile naming) — distinct from PA rule recipes
- ~~Generate summaries for NULL-summary repos in verisim-data~~ (DONE 2026-03-07: 295 summaries auto-generated)
- ~~Historical trend tracking across scan cycles~~ (DONE 2026-04-22: `lib/historical_trends.ex` + VCL.Query integration)
- ~~VCL federation executor — multi-store~~ (DONE 2026-04-22: `lib/vcl/remote_executor.ex`; `FROM FEDERATION REMOTE IN [...]`)

**Planned:**
- GraphQL API as live HTTP endpoint
- SARIF output for IDE integration
- Nx/EXLA backend for the neural layer if/when reservoir sizes outgrow pure Elixir
- Cross-organization federation with VCL drift policies

### Known Gaps

1. **verisim-api not deployed:** VeriSimDB Rust core not running — graph/vector/temporal modalities via flat files only
2. **Fix script coverage:** 310/600 auto-execute entries have null fix_script — recipes exist but no executable script
3. **Containerfiles:** Haskell still uses non-Chainguard base images (no Chainguard equivalents)
4. **Ada TUI not integrated:** Compiles but not wired into Elixir supervision tree
5. **Neural state persistence:** State dir exists but coordinator hasn't persisted to disk yet
6. **Neural training data balance:** ~99% success in the historical outcome log. Mitigated 2026-04-22 by `lib/neural/rebalancer.ex` (synthetic regressions + rule-based RBF targets, on by default); the synthetic path is Strategy A. Strategies B (adversarial perturbation) and C (real failure corpus from panic-attack history) remain unstarted.

## Code Style

- Elixir: `mix format`, SPDX headers on all files
- Rust: `rustfmt`, `clippy`
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

## Scanner Hygiene (preventing FPs at source)

Hypatia self-scans, so anything below is also how *Hypatia* avoids
re-introducing false positives into its own scan results. See the
post-mortem on PR #237 for the canonical examples.

### When you add a fixture / training corpus / remediation script

Default exemptions in `lib/hypatia/scanner_suppression.ex` cover:
`.audittraining/`, `test/`, `tests/`, `integration/fixtures/`,
`scripts/fix-scripts/`, the rule definition files themselves. **If your
new file belongs to one of those categories but lives elsewhere, add it
to that list** rather than baselining the resulting findings.

### When you write a workflow

* Push GitHub context into env first, then jq with `--arg` — never
  string-interpolate `${{ github.* }}` directly into a `run:` shell line.
* `${{ secrets.X }}` and `${{ vars.X }}` are *references*, not leaks; the
  scanner already exempts them, but use them rather than inline literals.
* Prefer args-list form for action `run:` blocks over shell strings.

### When you call out to a process from Elixir

* `System.cmd("bin", ["arg1", "arg2"])` — **safe**, no shell. Interpolating
  into the args list is fine.
* `System.shell("…#{x}")` — **always shell injection**. Don't.
* `:os.cmd('…#{x}')` — **always shell injection**. Don't.
* `Port.open({:spawn, "string"}, …)` — shell form. Use `:spawn_executable`
  + `args:` instead.

### When you write Rust in this repo

* `.unwrap()` is acceptable in `cli/`, `main.rs`, `build.rs`, `bin/*.rs`,
  `tools/`, `fixer/` — scanner downgrades those automatically.
* `.unwrap()` in library code (`adapters/`, `data/`, `integration/src/`)
  is real and should be migrated to `?` or `.unwrap_or_else(…exit…)`.
* For test fixture credentials, prefix the literal with `test-`,
  `dummy-`, `fake-`, `example-`, or `placeholder-` so the secret scanner
  recognises it.

### When you need to suppress a real finding

Three mechanisms, in order of preference:

1. **Fix the code.** Most "unsuppressable" findings are actually
   fixable in 5 minutes.
2. **Inline directive** at the call site, with a reason:
   ```
   let pw = "x"  // hypatia: allow security_errors/secret_detected -- doctest
   ```
   Recognised in `#`, `//`, `--`, `;` comment styles. A file-level
   directive in the first 20 lines covers every match in the file.
3. **`.hypatia-ignore`** for file-scoped or directory-scoped exemptions
   that have a documented org-policy rationale.

`.hypatia-baseline.json` should be a **last resort**. Baseline entries
are accepted findings — every new agent reads them as historical risk.
Prefer fix > inline directive > .hypatia-ignore > baseline.
