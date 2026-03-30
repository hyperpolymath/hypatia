# TEST-NEEDS.md — hypatia

> Generated 2026-03-29 by punishing audit.

## Current State

| Category     | Count | Notes |
|-------------|-------|-------|
| Unit tests   | 32    | Elixir: recipe_matcher, triangle_router, fleet_dispatcher, neural, vql, safety, etc. |
| Integration  | 7     | Rust: ci_simulation, arangodb, fleet, forge, hooks, registry + adapter/seam tests |
| E2E          | 0     | stress-test.yml workflow exists but no actual E2E test suite |
| Benchmarks   | 1     | `benches/hypatia_bench.rs` — **PLACEHOLDER** (`black_box(42)`, no real logic) |

**Source modules:** ~66 Elixir modules (15 rules, 7 neural networks, VQL, safety, data, fleet) + Rust adapters + 10 Idris2 ABI + 5 Zig FFI.

## What's Missing

### P2P (Property-Based) Tests
- [ ] Recipe matching: fuzz arbitrary repo structures against rule engine
- [ ] Neural network outputs: property tests for bounded confidence scores
- [ ] VQL query parsing: arbitrary query string fuzzing
- [ ] Pattern registry: invariant tests for pattern storage/retrieval

### E2E Tests
- [ ] Full scan pipeline: repo clone -> analysis -> recipe match -> action dispatch
- [ ] Fleet dispatch: multi-bot coordination round-trip
- [ ] VeriSimDB connector: write/read/query lifecycle
- [ ] Learning scheduler: full training cycle

### Aspect Tests
- **Security:** No tests for quarantine module, rate_limiter bypass, batch_rollback authorization — ZERO security-specific tests
- **Performance:** Benchmark is literally `black_box(42)`. No real scanning performance measurement
- **Concurrency:** No tests for parallel recipe evaluation, fleet race conditions, neural network concurrent inference
- **Error handling:** No tests for ArangoDB connection failure, malformed repo structure, VQL parse errors

### Build & Execution
- [ ] `mix test` runner verification
- [ ] `cargo test` for Rust adapters
- [ ] Zig FFI integration tests
- [ ] Container integration test (compose.test.yaml exists but needs validation)

### Benchmarks Needed
- [ ] Recipe matching latency per repo size
- [ ] Neural network inference time (each of 7 networks)
- [ ] Pattern analysis throughput
- [ ] VQL query execution time
- [ ] Full scan pipeline end-to-end latency

### Self-Tests
- [ ] Rule module self-validation (15 rules, none self-tested)
- [ ] Neural model loading verification
- [ ] ArangoDB schema migration check
- [ ] VeriSimDB connection health

## Priority

**HIGH.** 66 modules with 32 unit tests = 48% coverage by file, which sounds okay but hides the fact that 15 rule modules, 7 neural networks, and the entire safety subsystem lack dedicated tests. The benchmark file is a fraud — it measures nothing. A CI intelligence system that cannot benchmark its own performance is ironic.
