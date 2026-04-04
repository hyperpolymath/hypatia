# TEST-NEEDS.md — hypatia

## CRG Grade: B — ACHIEVED 2026-04-04

> Generated 2026-03-29 by punishing audit. Updated 2026-04-04 after CRG C blitz.
> CRG B achieved 2026-04-04: Scanned 6 diverse external repos with real output.

## CRG B Evidence — External Targets

| Target Repo | Language | What Was Tested | Result |
|-------------|----------|-----------------|--------|
| gossamer | Gleam/Rust/Idris2 | Full `hypatia scan` — security, policy, workflow audit | 70 findings (12 CRITICAL, 7 HIGH, 47 MEDIUM) |
| protocol-squisher | Rust (29 crates) | Full `hypatia scan` — code safety, banned languages | 209 findings (18 CRITICAL, 16 HIGH, 175 MEDIUM) |
| burble | Elixir/ReScript/Idris2 | Full `hypatia scan` — believe_me detection, deprecated APIs | 59 findings (2 CRITICAL, 5 HIGH, 48 MEDIUM) |
| stapeln | Idris2/Zig/Rust/ReScript | Full `hypatia scan` — container system, multi-language | 492 findings (34 CRITICAL, 209 HIGH, 249 MEDIUM) |
| boj-server | ReScript/Deno/Idris2 | Full `hypatia scan` — MCP server, workflow audit | 73 findings (11 CRITICAL, 7 HIGH, 51 MEDIUM) |
| standards | Mixed (Rust/ReScript/Nickel/TS) | Full `hypatia scan` — monorepo, multi-standard | 163 findings (30 CRITICAL, 46 HIGH, 87 MEDIUM) |

### Target Details

**1. gossamer (Gleam/Rust — window manager)**
- Command: `hypatia scan /var/mnt/eclipse/repos/gossamer --format text --severity low`
- Key findings: 7 banned TypeScript test files, 4 `believe_me` in Idris2 ABI proofs, `unwrap_or(0)` in Rust bindings, missing scorecard/dependabot workflows

**2. protocol-squisher (Rust — 29-crate workspace)**
- Command: `hypatia scan /var/mnt/eclipse/repos/protocol-squisher --format text --severity low`
- Key findings: 9 banned Python files (PyO3 integration), 2 proof holes (Admitted + sorry), 8 `unwrap_or(0)` across crates, 16 `unwrap()` DoS risks in examples/benches

**3. burble (Elixir — WebRTC comms)**
- Command: `hypatia scan /var/mnt/eclipse/repos/burble --format text --severity low`
- Key findings: 3 `believe_me` in Idris2 ABI, deprecated `Js.Array2`/`Js.Dict`/`Js.String2` in ReScript signaling module, missing scorecard workflow

**4. stapeln (Idris2/Zig — container system)**
- Command: `hypatia scan /var/mnt/eclipse/repos/stapeln --format text --severity low`
- Key findings: 8 banned TypeScript files, 8 `believe_me` + 5 `unsafePerformIO` across cerro-torre/vordr verification, 14 `getExn` in svalinn ReScript, 15 `unwrap_or(0)` in Rust runtime

**5. boj-server (ReScript/Deno — MCP server)**
- Command: `hypatia scan /var/mnt/eclipse/repos/boj-server --format text --severity low`
- Key findings: 5 banned TypeScript test files, 6 `believe_me` in safety ABI (SafeHTTP, SafeCORS, etc.), 78 `unwrap()` in cartridge-minter tool

**6. standards (Mixed — multi-standard monorepo)**
- Command: `hypatia scan /var/mnt/eclipse/repos/standards --format text --severity low`
- Key findings: 4 banned Python files (avow/consent-aware), 7 banned TypeScript (a2ml/axel/k9-svc/lol/rsr-certifier), 7 JSON decode without validation in lol crawlers, HTTP URL in Nickel config

## Current State

| Category     | Count | Notes |
|-------------|-------|-------|
| Unit tests   | 32    | Elixir: recipe_matcher, triangle_router, fleet_dispatcher, neural, vql, safety, etc. |
| Integration  | 7     | Rust: ci_simulation, arangodb, fleet, forge, hooks, registry + adapter/seam tests |
| E2E          | 15    | `test/e2e_pipeline_test.exs` — all 6 pipeline stages wired with real verisimdb-data |
| P2P (property-based) | 26 | `test/p2p_recipe_vql_test.exs` — RecipeMatcher invariants, VQL parser totality, dispatch_strategy monotonicity |
| Concurrency  | 16    | `test/concurrency_test.exs` — parallel recipe evaluation, RateLimiter/Quarantine/VQL under load |
| Smoke        | 13    | `test/zig_ffi_smoke_test.exs` — Zig FFI source integrity, data path structure, dispatch strategy spec |
| Benchmarks   | 1 file (7 real benches) | `fixer/benches/hypatia_bench.rs` — catalog construction/lookup, SHA pins, scan result summary, fixer construction |

**Source modules:** ~66 Elixir modules (15 rules, 7 neural networks, VQL, safety, data, fleet) + Rust adapters + 10 Idris2 ABI + 5 Zig FFI.

**Total new tests added (2026-04-04):** 70 tests across 4 new files. All pass.

## CRG C Status

**Achieved 2026-04-04:**
- [x] Unit tests: 32 (existing, Elixir)
- [x] Integration tests: 7 (Rust adapters)
- [x] E2E tests: 15 (all 6 pipeline stages)
- [x] P2P property-based tests: 26 (RecipeMatcher + VQL invariants)
- [x] Concurrency tests: 16 (parallel evaluation, GenServer safety)
- [x] Smoke tests: 13 (Zig FFI, data path, ABI files)
- [x] Benchmarks: real (7 benches in fixer/benches/hypatia_bench.rs)
- [x] No mocks — all tests use real ExUnit contexts + verisimdb-data on disk

**Benchmark note:** The root `benches/hypatia_bench.rs` stub is intentional (workspace has no
`[package]`). Real benchmarks run via `cargo bench -p hypatia-fixer`.

## Known Pre-existing Failures (do not fix here)

- `recipe_matcher_test.exs`: 3 failures — `recipe-shell-quote-vars.json` is empty/corrupted in verisimdb-data
- `triangle_router_test.exs`: 1 failure — depends on the corrupted recipe above
- `safety_test.exs`: 1 flaky failure — RateLimiter enqueue timing under OTP load

## What's Still Missing (for CRG B+)

### P2P Tests
- [ ] Neural network outputs: property tests for bounded confidence scores (needs mock training data)
- [ ] Pattern registry: invariant tests for deduplication correctness at scale

### E2E Tests
- [ ] Fleet dispatch multi-bot round-trip (needs PAT with repo scope)
- [ ] Learning scheduler full training cycle (too slow for CI)

### Aspect Tests
- **Performance:** No in-process latency measurement (Elixir has no criterion equivalent in test suite)
- **Error handling:** No tests for ArangoDB connection failure (integration dependency)
- **Zig FFI runtime:** No tests calling compiled .so (Zig build not in CI yet)

### Build & Execution
- [x] `mix test` runner verification — passes (minus pre-existing data quality failures)
- [x] `cargo test` for fixer — passes
- [ ] Workspace `cargo test` — blocked by pre-existing `bincode 3.0.0` compile error
- [ ] Zig FFI integration tests — `zig build test` not wired to CI
- [ ] Container integration test (compose.test.yaml exists but needs validation)

### Self-Tests
- [ ] Rule module self-validation (15 rules, none self-tested)
- [ ] Neural model loading verification
