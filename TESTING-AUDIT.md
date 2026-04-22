# Testing-Taxonomy Audit

**Date:** 2026-04-22
**Standard:** `hyperpolymath/standards` —
  `testing-and-benchmarking/TESTING-TAXONOMY.adoc` v1.1.0 (2026-04-04)
**Scope:** Hypatia test suite, CI workflows, benchmark infrastructure,
  dogfooding state, 14-aspect coverage.

## TL;DR

Hypatia is at a **solid D / borderline C** on the Component Readiness
Grade scale — strong core category coverage (8 of 16 categories at
"strong"), mature security and reproducibility aspects, zero
`sorry`/`Admitted`/`believe_me` in the formal-proof set. The three
gaps that block a confident C are:

1. **No baselined benchmarks.** Criterion harness exists (`fixer/benches/hypatia_bench.rs`)
   but no baseline JSON, no CI gate, no Six Sigma thresholds.
2. **No mutation or fuzz coverage.** Both categories register as
   absent — no `mutant`, no `cargo-fuzz` harnesses, no CI jobs.
3. **No isolated regression suite.** `test/regressions/` does not
   exist; bug-tied tests live inline in feature-test modules.

Reaching C is a bounded-scope sequence (details in "Punch list" below);
reaching B is a multi-quarter effort dominated by mutation + fuzz +
chaos coverage.

## 1. Category coverage (16 / 16)

Primary category per test file. Evidence verified against `test/*.exs`
(39 files, ~8,659 LOC) and `.github/workflows/*.yml`.

| Category | Status | Representative evidence |
|----------|--------|-------------------------|
| **UT** (Unit) | strong | `code_safety_test.exs`, `pattern_registry_test.exs`, `triangle_router_test.exs`, `confidence_annealing_test.exs`, `recipe_matcher_test.exs` (12 files) |
| **P2P** (Point-to-Point) | strong | `p2p_recipe_vcl_test.exs` (486 LOC), `concurrency_test.exs` (337 LOC) |
| **E2E** | strong | `e2e_pipeline_test.exs` (349 LOC) — full VerisimConnector → PatternRegistry → TriangleRouter → DispatchManifest pipeline |
| **BLD** (Build) | strong | `ci.yml` exercises `cargo check/clippy/test`, Haskell registry, Logtalk engine |
| **EXE** (Execution) | strong | `zig_ffi_smoke_test.exs` (248 LOC), `vcl_client_test.exs` (582 LOC), `vcl_file_executor_test.exs` (642 LOC) |
| **REF** (Reflexive) | weak | `safety_test.exs` (RateLimiter health), `blackboard_test.exs` (ETS consistency) — no dedicated self-diagnostic suite |
| **LCY** (Lifecycle) | strong | `kin_test.exs` (GenServer / Gate / Arbiter / Contingency), `training_pipeline_test.exs` (neural state evolution) |
| **SMK** (Smoke) | strong | `zig_ffi_smoke_test.exs`, `root_hygiene_test.exs`, `green_web_test.exs`, recipe smoke suites |
| **PBT** (Property-Based) | weak | `p2p_recipe_vcl_test.exs` uses parameterised tables but no `StreamData`; no 1000+-case coverage |
| **MUT** (Mutation) | **absent** | No `mutant` / `elixir_mutant` dep, no mutation workflow, no mutation-score tracking |
| **FUZ** (Fuzz) | **absent** | No `fuzz/` directory, no `cargo-fuzz` targets, no fuzz workflow. `.audittraining/` refs libfuzzer but no active harnesses |
| **CTR** (Contract) | weak | Implicit invariants in P2P tests; no Dust/K9/ADJUST runtime decorators, no assertion harness |
| **REG** (Regression) | **absent** | No `test/regressions/` subdirectory; no issue-tied test case files |
| **CHS** (Chaos) | weak | Concurrency stress under load; no deliberate failure injection, network-partition simulation, or crash scenarios |
| **CMP** (Compatibility) | weak | `recipe_new_recipes_test.exs` covers backward co-existence only; no forward-compat, no deprecated-API tests |
| **PRF** (Proof Regression) | strong | `verify-tlaplus.yml` gates KinGate on every push + weekly; 9 Lean + Idris proofs on `claude/quality-gates` + `claude/lean-proofs-parser-pagerank` awaiting merge |

**Totals:** 8 strong · 5 weak · 3 absent.

## 2. DOG-01..DOG-10 dogfood-self

| Check | Status | Evidence / fix |
|-------|--------|----------------|
| **DOG-01** — CI gate fires | ✅ | `.github/workflows/hypatia-scan.yml` runs on PR, produces JSON with findings + PR comments |
| **DOG-02** — Annotation pipeline | ✅ | Findings carry `file`/`line`; enforced JSON format in workflow |
| **DOG-03** — Groove manifest | ✅ | `.well-known/groove/manifest.json` exists; ports match Elixir `:9090` |
| **DOG-04** — K9 contracts | ⚠ | `k9iser.toml` present, but `k9iser build` not invoked in CI, no `generated/k9iser/` artefacts. **Fix:** add a build step to `ci.yml`. |
| **DOG-05** — VeriSimDB wired | ✅ | `lib/verisim_connector.ex` actually calls `Hypatia.VCL.Client`; not a hollow import |
| **DOG-06** — Template placeholders | ⚠ | `{{OWNER}}` / `{{REPO}}` strings live only in `lib/rules/dogfooding.ex` as rule data, not as unresolved repo config. **Fix:** add a comment marking them as in-rule string literals; optional. |
| **DOG-07** — Tool version currency | ✅ | Actions SHA-pinned with v-tag comments; current as of 2026-04 |
| **DOG-08** — Consumer field contract | ✅ | Scanner JSON fields match consumer scripts |
| **DOG-09** — Iser applicability | ❌ | No `.machine_readable/dogfooding.a2ml`. **Fix:** add the manifest declaring which isers apply (yes/no/deferred). |
| **DOG-10** — Naming currency | ✅ | Only stale reference is `test/p2p_recipe_vql_test.exs` filename (test body uses `vcl`); intentional legacy-audit paths kept |

**Score:** 7/10 clean, 2 warnings (DOG-04, DOG-06), 1 failure (DOG-09).

## 3. Six Sigma benchmark infrastructure

| Category | Baseline | CI-gated | Status |
|----------|----------|----------|--------|
| Latency (p50/p95/p99/p999) | ✗ | ✗ | partial — `fixer/benches/hypatia_bench.rs` exists (7 criterion benches) |
| Throughput | ✗ | ✗ | partial — same file |
| Memory (steady + peak) | ✗ | ✗ | scaffolded — `stress-test.yml` uses valgrind weekly |
| Startup | ✗ | ✗ | absent — no `Instant` measurements |
| Build time | ✗ | ✗ | absent — `cargo build --release` runs but untimed |
| Energy | ✗ | ✗ | absent — no power profiling anywhere |
| FFI overhead | ✗ | ✗ | absent — `ffi/zig/` has functional tests only |

**Overall benchmark-readiness verdict:** **scaffolded**. Framework
installed, a handful of benches compile, nothing is recorded or
gated. No baseline JSON in `.machine_readable/benchmarks/` (directory
does not exist).

**Path to "partial":** run criterion on main 10 times, commit median
baseline, add a CI job that fails if p95 regresses > 50%.

## 4. Aspect-dimension coverage (14 / 14)

| Aspect | Status | Representative evidence |
|--------|--------|-------------------------|
| **Security** | strong | `security.yml`, cargo-audit, cargo-deny, CodeQL, secret scanner, SBOM generation, Dependabot |
| **Safety** | strong | 0 `believe_me`/`sorry`/`Admitted`/`postulate` in `verification/` + `src/abi/` + `verify/`; `%default total` in every Idris 2 file |
| **Interoperability** | strong | Multi-forge adapters (GitHub / GitLab / Bitbucket / Codeberg / sr.ht), REST/GraphQL/gRPC in Idris 2 ABI, VCL protocol |
| **Functionality** | strong | 39 Elixir + 38 Rust test files, ~8,659 Elixir LOC, full pipeline E2E |
| **Versability** | strong | SemVer in `CHANGELOG.adoc` + `mix.exs`, detailed per-version changelogs |
| **Maintainability** | strong | Modular Rust workspace, TOPOLOGY.md, DESIGN-NARRATIVE.md, clean module boundaries |
| **Reproducibility** | strong | `flake.nix` + `guix.scm`, `Cargo.lock` + `mix.lock` committed |
| **Portability** | strong | Cross-platform CI matrix (Linux x86_64 / ARM64, macOS x86_64 / ARM64, Windows), Containerfile, podman |
| **Dependability** | weak | OTP supervision + concurrency tests but no chaos engineering, no fault-injection |
| **Performance** | weak | No published metrics, no latency / throughput benchmarks gated, no capacity planning |
| **Observability** | weak | 41 source files use `Logger.*`; no Prometheus / OpenTelemetry export, no structured metrics pipeline |
| **Usability** | absent | No systematic error-messaging audit, no `just doctor` coverage report, no UX review |
| **Accessibility** | absent | No WCAG 2.3 audit, no `ADJUST.contractile`, Gossamer UI not yet checked against 54 accessibility checks |
| **Privacy** | absent | No GDPR mapping, no PRIVACY.md, data-flow for findings / outcomes not documented |

**Totals:** 8 strong · 3 weak · 3 absent.

## 5. CRG grade derivation

Per the CRG-grade ↔ test-evidence mapping in TESTING-TAXONOMY.adoc:

| Grade | Requirements | Hypatia status |
|-------|--------------|----------------|
| **D** | Unit + smoke + test matrix + RSR compliance | ✅ all met |
| **C** | D + P2P + E2E + reflexive + build + contract + aspect + benchmarks baselined + deep annotation | ⚠ partial — P2P ✓, E2E ✓, reflexive weak, build ✓, contract weak, aspect mostly ✓, **benchmarks not baselined** ✗, annotation ✓ |
| **B** | C + property-based + fuzz + compatibility + lifecycle, mutation > 80%, benchmarks in Ordinary range across 6+ targets | ✗ fuz absent, mutation absent, pbt weak, cmp weak |
| **A** | B + chaos + regression suite + stable benchmarks across releases + external feedback | ✗ chaos weak, regression absent |

**Current grade: D+ (with a line of sight to C).** The C gap is
dominated by benchmark baselining + contract harness + hardening
reflexive/compatibility. None of these are research; all are
finishable in bounded time.

## 6. Punch list — ordered by value-per-effort

Each item estimated in calendar-days assuming one focused contributor.

### Tier 1 — reach C (≈ 1 sprint)

1. **Bench baseline + CI gate.** Record 10 criterion runs on main,
   commit median to `.machine_readable/benchmarks/baselines.json`,
   add a `cargo bench --save-baseline` step to `ci.yml` that fails
   if p95 regresses > 50%. **Effort: 2 days.**
2. **Contract test harness.** Add `.contractile` assertion decorators
   to the three most critical GenServers (VCL Client, RateLimiter,
   Quarantine). One test file per harness. **Effort: 1 day.**
3. **Reflexive test suite.** Consolidate ad-hoc health checks into
   `test/reflexive_test.exs` that covers the `just doctor` recipe,
   VCL cache consistency, supervision-tree invariants. **Effort: 1 day.**
4. **Create `.machine_readable/dogfooding.a2ml`** (DOG-09 fix).
   Declare applicability of DOG-01..DOG-10 per item. **Effort: 2 hours.**
5. **Wire k9iser build into CI** (DOG-04 fix). **Effort: 2 hours.**

### Tier 2 — reach B (≈ 1 quarter)

6. **Property-based tests with StreamData.** Target VCL parser +
   pattern registry + recipe matcher (three property sets, ≥ 1000
   cases each). **Effort: 1 week.**
7. **Fuzz targets.** Add `fuzz/fuzz_targets/` with `cargo-fuzz`
   targets for VCL parser + JSON deserialisation + Zig FFI boundary.
   One CI job (`fuzz.yml`) that runs each target for 2 minutes on
   merge to main. **Effort: 1 week.**
8. **Mutation score pipeline.** Adopt `mutant` (or `muzak`) for
   Elixir, `cargo-mutants` for Rust. Target > 80% mutation score on
   `lib/rules/`, `lib/neural/`, and `fixer/src/`. **Effort: 2 weeks.**
9. **Compatibility suite.** Version-boundary tests: old recipes
   parse in current version, migrations are reversible, deprecated
   public APIs emit warnings. **Effort: 3 days.**
10. **Lifecycle tests for every GenServer.** The Kin lifecycle is
    covered; RateLimiter, Quarantine, BatchRollback, Neural
    Coordinator, LearningScheduler are not. **Effort: 3 days.**

### Tier 3 — reach A (multi-quarter)

11. **Chaos / resilience tests.** Deliberate failures:
    network-partition, GenServer crash-under-load, ETS corruption,
    outcomes-log write failure. **Effort: 2 weeks.**
12. **Isolated regression suite.** `test/regressions/gh_<issue>_<slug>.exs`
    convention; backfill from `CHANGELOG.adoc` fix entries. **Effort:
    1 week (initial) + ongoing discipline.**
13. **Privacy + accessibility compliance.** Dedicated efforts; see
    Tier 3 items in the aspect table. **Effort: 2 weeks each.**

### Not in this tier

* **Observability upgrade (OpenTelemetry / Prometheus).** Already
  tracked in CLAUDE.md "Planned" — orthogonal to test grading.
* **Full PageRank convergence proof (Mathlib).** Tracked in
  `PROOFS-GAP.md` PR 4.

## 7. Out of scope

- **External user feedback** (required for A grade). Can't be
  manufactured; depends on adoption.
- **Formal WCAG / GDPR audit.** Requires domain expertise we don't
  have in-house; best addressed by external reviewer once Gossamer
  UI is compiled and deployed.
- **Energy profiling.** No tooling currently exists for BEAM + Rust
  hybrid; wait for upstream primitives.

## 8. Appendix — file inventory by category

Full mapping in `.machine_readable/testing/blitz.a2ml`.
