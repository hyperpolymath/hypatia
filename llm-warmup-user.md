<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- LLM warmup context — USER level (<200 lines) -->
<!-- Feed this to an LLM before asking questions about Hypatia -->

# Hypatia — User Context

## What it is

Hypatia is the neurosymbolic CI/CD intelligence layer for the hyperpolymath
ecosystem. It scans repositories for weak points, routes findings through a
safety triangle (Eliminate > Substitute > Control), and dispatches fixes to a
fleet of bots with Bayesian confidence scoring.

## Architecture overview

```
panic-attack assail (scan repos)
        |
verisimdb-data (git-backed flat-file store)
        |
Elixir pipeline:
  PatternRegistry → TriangleRouter → FleetDispatcher
        |
  5 neural networks (trust, experts, anomaly, forecast, similarity)
        |
dispatch-runner.sh (gitbot-fleet)
  ├── auto_execute (>=0.95 confidence)
  ├── review (0.85-0.94)
  └── report_only (<0.85)
        |
OutcomeTracker (Bayesian feedback loop)
```

## Key files

| Path | Purpose |
|------|---------|
| `lib/pattern_analyzer.ex` | Full pipeline orchestrator |
| `lib/verisimdb_connector.ex` | VQL-powered data access |
| `lib/pattern_registry.ex` | 954 canonical patterns (PA001-PA020) |
| `lib/triangle_router.ex` | Safety triangle routing |
| `lib/fleet_dispatcher.ex` | Confidence-gated dispatch |
| `lib/outcome_tracker.ex` | Bayesian confidence updating |
| `lib/neural/` | 5 neural networks + coordinator |
| `lib/vql/` | VQL query layer (parser + file executor) |
| `lib/safety/` | Rate limiter, quarantine, batch rollback |
| `adapters/` | Rust adapter crate |
| `cli/` | Rust CLI crate |
| `src/abi/` | Idris2 ABI definitions |
| `ffi/zig/` | Zig FFI bridge (7 functions) |
| `mix.exs` | Elixir project config |
| `Cargo.toml` | Rust workspace config |

## Quick commands

```bash
just build-all       # Build Rust + Elixir
just test            # Rust tests
just test-elixir     # Elixir tests
just scan <path>     # Scan a repository
just doctor          # Check prerequisites
```

Prerequisites: Elixir (>= 1.14), Erlang/OTP (>= 25), Rust (nightly), just.

## Neural networks

| Network | Type | Purpose |
|---------|------|---------|
| Graph of Trust | PageRank | Trust-weighted routing |
| Mixture of Experts | Sparse MoE | Domain-specific confidence (7 domains) |
| Liquid State Machine | Reservoir | Temporal anomaly detection |
| Echo State Network | Reservoir | Confidence trajectory forecasting |
| Radial Neural Network | RBF | Similarity + novelty detection |

## Safety systems

- **Rate limiter**: Per-bot (50/min), global (200/min), burst (10/5s)
- **Quarantine**: Auto-quarantine on 5+ failures or >30% FP rate
- **Batch rollback**: Rollback entire dispatch batches with confidence revert

## Dispatch confidence tiers

| Confidence | Action | Executor |
|------------|--------|----------|
| >= 0.95 | auto_execute | robot-repo-automaton |
| 0.85-0.94 | review | rhodibot creates PR |
| < 0.85 | report_only | sustainabot advisory |

## Gitbot fleet

- **rhodibot** — Git operations, PR creation
- **echidnabot** — Code quality verification
- **sustainabot** — Dependency updates, advisories
- **glambot** — Documentation beautification
- **seambot** — Integration testing
- **finishbot** — Task completion

## Metrics (as of 2026-03-07)

- 302 repos scanned, 3385 weak points
- 1635 dispatched actions (600 auto, 667 review, 368 report)
- 16671 outcomes (99% success rate)
- 46 recipes, 20 OpenSSF Scorecard recipes, 5 RSR compliance rules

## License

PMPL-1.0-or-later. Author: Jonathan D.A. Jewell.

## Ecosystem position

- **Depends on**: panic-attacker (scanning), verisimdb-data (data), proven (verified safety)
- **Coordinates**: gitbot-fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot)
- **Downstream**: All hyperpolymath repos benefit from automated scanning + fixing
