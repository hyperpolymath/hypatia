# TEST-NEEDS.md — hypatia

> Generated 2026-03-29 by punishing audit. Updated 2026-04-04 after CRG C blitz.

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
