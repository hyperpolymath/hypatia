# CLAUDE.md - Hypatia AI Assistant Instructions

## Project Overview

Hypatia is the neurosymbolic CI/CD intelligence layer for the hyperpolymath ecosystem. It coordinates the gitbot-fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot, panicbot) via a safety triangle pipeline, with 8 neural networks on a shared blackboard for intelligent dispatch, verisim-data (git-backed canonical flat-file store) with VCL queries, Bayesian confidence updating, and 33 Elixir rule modules for pattern detection.

> **Counts in this file are load-bearing — agents act on them.** They were re-measured
> against the tree on 2026-08-07. If you change the shape of the system, re-measure
> (`ls lib/**/*.ex | wc -l`, `grep -c "export fn" ffi/zig/src/main.zig`, the child list in
> `lib/application.ex`) rather than editing a number by eye. Known-stale claims and the
> full audit live in `docs/DEBT-REGISTER.md`.

## Architecture

```
Hypatia                       # 133 Elixir modules total
├── Elixir pipeline          # core: pattern analysis, dispatch, learning
├── Merge orchestration       # 12 modules — leases, ledger, ticker (largest subsystem)
├── Kin subsystem             # 6 modules — contingency, arbiter, gate, watchdog
├── Mix tasks                 # 15 `mix hypatia.*` tasks (rsr_score, watch, triage_issues, …)
├── Web tier                  # 9 modules — dashboard, SSE, Prometheus /metrics, GraphQL, SARIF
├── Neural subsystem          # 8 networks on a blackboard + coordinator GenServer
│   ├── Graph of Trust        # PageRank trust over repos/bots/recipes
│   ├── Mixture of Experts    # Domain-specific confidence (7 expert domains)
│   ├── Liquid State Machine  # Temporal anomaly detection
│   ├── Echo State Network    # Confidence trajectory forecasting
│   ├── Radial Neural Network # Finding similarity + novelty detection
│   ├── Graph Neural Network  # Structural learning over the repo graph
│   ├── Variational Autoenc.  # Latent finding representations
│   └── Sequence Model        # Sequential dispatch modelling
├── VCL query layer            # 7 modules: parser, file executor, cache, federation, cross-org
├── Data layer                 # verisim-data (canonical flat-file store)
├── Safety systems             # Rate limiter, quarantine, batch rollback (see Known Gaps — inert)
├── OTP Application           # 24 supervised children (+2 conditional) — see lib/application.ex
├── Elixir rules engine       # 33 rule modules (lib/rules/) — error catalog, pattern detection
├── Idris2 ABI               # Types, GraphQL, gRPC, REST with dependent type proofs
├── Zig FFI                   # C ABI bridge (18 exported functions) + 16 protocol connectors
├── Rust workspace            # adapters, cli, data, fixer, integration
├── Safety triangle           # Eliminate > Substitute > Control
├── Fleet dispatcher          # File-based + HTTP dispatch with circuit breaker
└── Integration connectors    # verisim-data, panic-attack, gitbot-fleet
```

## Key Commands

`just` is the primary entry point (39 recipes; `CONTRIBUTING.md` treats it as canonical):

```bash
just --list           # every recipe
just doctor           # toolchain preflight
just build-all        # Elixir + Rust + Zig FFI + Idris2 ABI
just test-elixir      # the Elixir suite
just fmt-check        # formatting gate
```

Underlying commands:

```bash
mix deps.get    # Install Elixir deps
mix test        # Run tests — seed is PINNED to 0 in test/test_helper.exs (#643),
                # so runs are deterministic. Probe order-dependence with --seed N.
mix format      # Format Elixir code
cargo build     # Build Rust workspace
cargo test      # Test Rust workspace
```

`mix test` excludes 242 `:verisim_data` tests. They are **not** run in CI and do **not** pass —
see Known Gaps 3 before quoting a green suite.

## Machine-Readable Artefacts

Files in `.machine_readable/` contain structured project metadata:

- `.machine_readable/descriptiles/STATE.a2ml` - Current project state and progress
- `.machine_readable/descriptiles/META.a2ml` - Architecture decisions and development practices
- `.machine_readable/descriptiles/ECOSYSTEM.a2ml` - Position in the ecosystem and related projects
- `.machine_readable/descriptiles/AGENTIC.a2ml` - AI agent interaction patterns
- `.machine_readable/descriptiles/NEUROSYM.a2ml` - Neurosymbolic integration config
- `.machine_readable/descriptiles/PLAYBOOK.a2ml` - Operational runbook

## Safety Triangle Pipeline (OPERATIONAL)

### Data Flow

```
panic-attack assail (scan repos)
        | JSON results
verisim-data repo (git-backed flat-file store)
        | read scan results
Elixir pipeline:
  VerisimConnector.fetch_all_scans()
        |
  PatternRegistry.sync_from_scans()     -- dedupes findings into canonical patterns
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
| `verisim_connector.ex` | VCL-powered data access with file I/O fallback (renamed per ADR-002; the old `verisimdb_connector.ex` name is gone) |
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
| `remote_executor.ex` | Federation executor — `FROM FEDERATION REMOTE IN [...]` |
| `remote_cache.ex` | Caches federated query results |
| `cross_org.ex` | Cross-organisation federation with drift policies |
| `proof_resolver.ex` | Proof-obligation resolution (currently unreferenced — see DEBT-REGISTER C-5) |

### Neural Network Modules (lib/neural/)

| Module | Type | Purpose |
|--------|------|---------|
| `graph_of_trust.ex` | PageRank | Trust-weighted routing over repos/bots/recipes |
| `mixture_of_experts.ex` | Sparse MoE | Domain-specific confidence (7 expert domains) |
| `liquid_state_machine.ex` | Reservoir | Temporal anomaly detection in event streams |
| `echo_state_network.ex` | Reservoir | Confidence trajectory forecasting + drift detection |
| `radial_neural_network.ex` | RBF | Finding similarity, novelty detection, classification |
| `graph_neural_network.ex` | GNN | Structural learning over the repo/finding graph |
| `variational_autoencoder.ex` | VAE | Latent representations of findings |
| `sequence_model.ex` | Sequence | Sequential dispatch modelling |
| `blackboard.ex` | ETS | Shared board all 8 networks read/write, six phases |
| `coordinator.ex` | GenServer | Orchestrates all 8 networks over the blackboard |
| `persistence.ex` | — | Warm-restart state for all 8 networks |
| `rebalancer.ex` | — | Training-data rebalancing, strategies A/B/C |
| `prover_recommender.ex` | — | Recommends a prover for a proof obligation |

### Neural Training (lib/neural/)

| Module | Purpose |
|--------|---------|
| `training_pipeline.ex` | ESN/RBF training from real verisim-data outcomes + pattern vectors |

Training pipeline reads outcomes/*.jsonl for ESN (confidence time series) and patterns/registry.json for RBF (8-D feature vectors). Coordinator's `:force_cycle` triggers training automatically.

### Idris2 ABI (src/Hypatia/ABI/)

| File | Purpose |
|------|---------|
| `Types.idr` | Core types with dependent type proofs |
| `GraphQL.idr` | Query/Mutation/Subscription operations with proofs |
| `GRPC.idr` | gRPC service definitions (scanner, dispatch, stream, health) |
| `REST.idr` | REST endpoint definitions (18 endpoints, 6 groups) |
| `FFI.idr` | GADT constructors for all C ABI functions + ffiReturnsApiResponse proof |
| `RuleEngine.idr` | Rule-evaluation types |

**Build system:** `src/abi/hypatia-abi.ipkg` (compile), `verify/hypatia-verify.ipkg` (proofs), `pack.toml` (Pack package manager).
The ipkg sets `sourcedir = ".."`, so **`src/Hypatia/ABI/` is what compiles**. A byte-identical
copy of all six modules also sits in `src/abi/*.idr` and is built by nothing — divergence between
them is undetectable. See DEBT-REGISTER C-4.

### Zig FFI (ffi/zig/src/)

| Function | Purpose |
|----------|---------|
| `hypatia_init` / `hypatia_free` | Lifecycle: allocate and release the handle |
| `hypatia_is_initialized` | Handle-state predicate |
| `hypatia_process` / `hypatia_process_array` | Core processing entry points |
| `hypatia_get_string` / `hypatia_free_string` | String marshalling across the ABI |
| `hypatia_last_error` | Last error for the calling thread |
| `hypatia_version` / `hypatia_build_info` | Version and build provenance |
| `hypatia_register_callback` | Host callback registration |
| `hypatia_health_check` | Health status of all components |
| `hypatia_scan_repo` | Trigger scan for repository |
| `hypatia_dispatch` | Dispatch finding to fleet |
| `hypatia_record_outcome` | Record fix outcome |
| `hypatia_force_learning_cycle` | Force learning cycle |
| `hypatia_get_confidence` | Get recipe confidence |
| `hypatia_dispatch_strategy` | Map confidence to dispatch strategy |

18 exports in `ffi/zig/src/main.zig`, plus 16 protocol connectors under
`ffi/zig/src/connectors/`. **No CI job builds or tests any of it** — `just build-ffi` exists but
is never invoked by a workflow (DEBT-REGISTER P-3).

### Data Layer

verisim-data (git-backed flat files) is the canonical data store. VCL queries execute against it directly via FileExecutor. Neural state persists to `data/verisim/neural-states/`. Outcomes append to `outcomes/YYYY-MM.jsonl`.

### Safety Systems (lib/safety/)

| Module | Purpose |
|--------|---------|
| `rate_limiter.ex` | Per-bot (50/min), global (200/min), burst (10/5s) dispatch limits |
| `quarantine.ex` | Auto-quarantine on 5+ failures or >30% FP rate; 3 levels (soft/hard/permanent) |
| `batch_rollback.ex` | Rollback entire dispatch batches with confidence revert |

### Metrics — generate them, do not quote them

There used to be a block of hard numbers here ("302 repos scanned, 3385 weak points, 16671
outcomes…"). It had **no producer**, so it was copied into five documents and drifted into
*six mutually inconsistent repo counts* (283 / 292 / 298 / 300 / 302 / 407). It has been removed
rather than re-dated.

If you need a current figure, run the thing that computes it:

```bash
mix hypatia.recipe_health              # per-recipe verification rates, quarantine candidates
mix hypatia.rsr_score . --ssot test/fixtures/a2ml/rsr-criteria-v2.a2ml   # RSR conformance
mix hypatia.strategy_effectiveness     # rebalancer strategy comparison
mix hypatia.watch                      # live counters; also /metrics (Prometheus) and /api/status
```

Note the `--ssot` flag is currently **required** — the default path resolution is broken
(DEBT-REGISTER M-3).

Design constants that are genuinely stable (these are code, not measurements):

- Bayesian Beta-distribution confidence: `prior_strength=10`, `floor=0.10`, `cap=0.99`
- Dispatch tiers: `>=0.95` auto_execute, `0.85-0.94` review, `<0.85` report_only
- Rate limits: per-bot 50/min, global 200/min, burst 10/5s
- Quarantine: 5 consecutive failures, or >30% FP rate over >=5 outcomes

### Remaining Work (M7+: Production Operations)

**Critical:**
- ~~Create PAT with repo scope for automated cross-repo dispatch~~ (DONE 2026-05-24: `HYPATIA_DISPATCH_PAT` provisioned + verified — 19 `hypatia-security-alert` events landed in gitbot-fleet from first manual sweep, all completing 17-26s)
- ~~Resolve "310 null-fix-script dispatch entries"~~ (DONE 2026-05-24, PR #309 commit `d2bbf75`: root cause was matcher language-gate, not missing scripts — all 22 scorecard fix scripts already existed on disk)
- Push committed fixes to remotes across repos (PAT now allows it; dispatch-runner side needs to call `mix hypatia.record_outcome` to populate the verification metric — `mix` task delivered in PR #309 commit `5e895b5`)

**Important:**
- Deploy verisim-api server (enables native graph/vector/temporal modalities)
- 5 new RSR compliance rules cover structural compliance (banned languages, SCM locations, required files, Containerfile naming) — distinct from PA rule recipes
- ~~Generate summaries for NULL-summary repos in verisim-data~~ (DONE 2026-03-07: 295 summaries auto-generated)
- ~~Historical trend tracking across scan cycles~~ (DONE 2026-04-22: `lib/historical_trends.ex` + VCL.Query integration; PR #309 adds 5-min snapshot persistence to `data/verisim/metrics/`)
- ~~VCL federation executor — multi-store~~ (DONE 2026-04-22: `lib/vcl/remote_executor.ex`; `FROM FEDERATION REMOTE IN [...]`)
- ~~Live watcher / supervision interface~~ (DONE 2026-05-24, PR #309: 10 commits across 3 phases — telemetry → Watcher GenServer → HTTP API + SSE + HTML dashboard + Prometheus + alerts + 5-min persistence + statistical anomaly detection)
- ~~Closed-loop quality~~ (DONE 2026-05-24, PR #309: soundness gates (in-process + escript-packaging) + closed-loop verification metric + auto-quarantine in FleetDispatcher)

**Still planned:**
- Nx/EXLA backend for the neural layer if/when reservoir sizes outgrow pure Elixir

*(M13 SARIF, M14 GraphQL endpoint, M15 bearer auth + persistent watcher + alert federation,
M9 rebalancer strategies B/C, and M10 Ada TUI wiring all shipped — `lib/hypatia/sarif.ex`,
`lib/hypatia/web/graphql.ex`, `lib/neural/persistence.ex`, `lib/tui/port.ex`. They were listed
here as "planned" for months after landing.)*

### Known Gaps

Re-verified 2026-08-07. Items 4-6 of the old list (Ada TUI unwired, neural state not persisted,
fix-script coverage) were **already resolved** and have been removed; they were misleading agents
into redoing finished work.

1. **verisim-api not deployed** — VeriSimDB Rust core not running; graph/vector/temporal
   modalities go through flat files only.
2. **The safety systems are inert.** `rate_limiter.ex`, `quarantine.ex` and `batch_rollback.ex`
   are fully-built GenServers that **no dispatch path actually calls**. Treat the "3 safety
   systems" line in any older doc as describing capability, not enforcement. This is the most
   dangerous stale claim the docs used to make.
3. **242 tests are dark and red.** The `:verisim_data` tag is excluded unconditionally in
   `test/test_helper.exs`, never included by any workflow, and `mix test --only verisim_data`
   yields **129 failures**. "The suite passes" is therefore scoped to 83% of it.
   (DEBT-REGISTER T-1.)
4. **Rust CI is entirely blocked** by a stale `dtolnay/rust-toolchain@stable` pin in
   `.github/workflows/actions.lock` — 13 jobs die at `Set up job`. (DEBT-REGISTER CI-1.)
5. **ESN training-data schema drift** — the echo-state forecaster has been learning from a
   shape it no longer receives.
6. **Silent scanner false-negatives** — wildcard recipes dropped, directory-mode dead patterns.
7. **Neural training-data balance** — ~99% success in the historical outcome log.
   `lib/neural/rebalancer.ex` mitigates via synthetic regressions (Strategy A); B and C now exist
   but their effectiveness is unmeasured outside `mix hypatia.strategy_effectiveness`.
8. **Containerfiles** — Haskell still uses non-Chainguard base images (no equivalents exist).
9. **Zig FFI is ungated** — 18 exports and 16 connectors, no `zig build` in any workflow.

The full evidence-backed audit, including CI/CD, licence, proof and metadata debt, is
`docs/DEBT-REGISTER.md`. Read it before trusting any count in an older document.

## Code Style

- Elixir: `mix format`, SPDX headers on all files
- Rust: `rustfmt`, `clippy`
- Shell: `ShellCheck`, POSIX-compatible
- SPDX-License-Identifier: CC-BY-SA-4.0

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

### When you edit a `path_allow_prefixes` list

These lists (on `@blocked_patterns` in `lib/rules/cicd_rules.ex`) are the **single source of
truth** for banned-language carve-outs across the estate; sibling repos' CLAUDE.md tables
mirror them. Two traps, both proven in production:

* **Never give an entry a leading slash unless you mean it.** Matching is
  `String.contains?/2` against paths that arrive *repo-root-relative*
  (`echidna/examples/x.v`), so `"/echidna/examples/"` can never match — it silently
  disables the exemption. Four vlang Coq/Verilog carve-outs were dead this way for months.
* **Do not hand-copy the list into a second module.** A duplicated table in
  `scanner_suppression.ex` drifted to 3 of ~12 carve-outs and flagged an exempt file
  Critical (standards#382). It has been deleted; delegate to
  `blocked_pattern_allow_match?/1` instead.

### Rust comment stripping (changed 2026-08-07)

`lib/rules/code_safety.ex` now strips whole-line `//` comments and whole-line `/* */` blocks
for Rust before scanning, so `// TODO: replace this .unwrap() with ?` no longer produces a
HIGH finding. Stripping is deliberately **line-anchored** — mid-line `//` inside string
literals (`"https://…"`) is preserved, because stripping it would corrupt code. If you add a
language, follow the same conservative shape (`strip_*_line_comments/1` in that module).

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
