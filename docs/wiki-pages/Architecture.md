<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Architecture

Hypatia is split into eleven layers, each supervised under the OTP `Hypatia.Supervisor`. The supervision tree starts in this exact order so each layer can depend on the ones below it.

## Supervision tree (top-down by start order)

```
Hypatia.Supervisor
├── Layer 0   HTTP  (Bandit + Hypatia.Web.Router on :9090)
├── Layer 0   VCL.Client                    (query parser + executor router)
├── Layer 0.5 Dispatch.Pipeline             (GenStage parallel dispatch)
├── Layer 0.7 Diagnostics.Monitor           (health + auto-recovery)
├── Layer 0.8 Watcher                       (telemetry → ETS rolling counts)
├── Layer 0.8 Watcher.PubSub                (SSE fan-out registry)
├── Layer 0.9 Watcher.Alerts                (threshold rules + sinks)
├── Layer 0.95 Watcher.Persistence          (5-min snapshots → verisim-data)
├── Layer 0.97 Watcher.AnomalyDetector      (statistical + ESN drift)
├── Layer 1   Safety.RateLimiter            (per-bot + global + burst limits)
├── Layer 1   Safety.Quarantine             (bot-level: failures + FP rate)
├── Layer 3   Rules.Learning                (rule-engine state)
├── Layer 3   LearningScheduler             (5-min cycle: outcomes → train)
├── Layer 3   SelfDiagnostics               (component health)
├── Layer 4   Neural.Blackboard             (shared ETS for the 8 networks)
├── Layer 4   Neural.GraphNeuralNetwork     (Phase 2 — agent interaction)
├── Layer 4   Neural.VariationalAutoencoder (Phase 5 — interpretation cluster)
├── Layer 4   Neural.SequenceModel          (Phase 6 — choreography predict)
├── Layer 4   Neural.Coordinator            (orchestrates 8 networks, 6 phases)
└── Layer 5   Kin                           (ecosystem coordination, watchdog)
```

## Pipeline data flow

```
panic-attack assail (scan repos)
     │ JSON results
verisim-data (git-backed flat-file store, ~290 repos)
     │ VCL queries via VCL.Client → FileExecutor
PatternRegistry.sync_from_scans (canonical patterns PA001-PA020+)
     │
TriangleRouter (Eliminate → Substitute → Control)
     │
FleetDispatcher (confidence-gated, auto-quarantine on low verification rate)
     │
DispatchManifest (JSONL bridge to bash dispatch-runner in gitbot-fleet)
     │
robot-repo-automaton / rhodibot / sustainabot
     │ outcomes JSONL
LearningScheduler ingest → OutcomeTracker (Bayesian update + re-scan verify)
     │ feedback
Neural.Coordinator (8 networks retrain every 5 minutes)
```

## Eight neural networks (blackboard, six phases)

| Network | Phase | Role |
|---|---|---|
| RBF | 1 | Novelty detection — novel vs known |
| PageRank (Graph-of-Trust) | 2 | Trust-weighted routing |
| GNN | 2 | Agent interaction graph |
| MoE | 3 | Domain-specific confidence (7 experts) |
| ESN | 4 | Confidence trajectory + drift detection |
| LSM | 4 | Temporal anomaly detection |
| VAE | 5 | Hermeneutic interpretation clustering |
| Sequence | 6 | Choreography trace prediction |

Each phase reads from the blackboard, computes, writes back. Parallel phases run concurrently via `Task.async_stream`. Confidence is **derived** from the full reasoning trace — no hardcoded fixed weights.

## VCL — the query layer

Small SQL-like language over the canonical verisim-data store. Examples:

```vcl
SELECT recipe_id, AVG(confidence) FROM outcomes WHERE bot = "rhodibot" GROUP BY recipe_id;

FROM FEDERATION REMOTE IN ["https://hypatia.peer1", "https://hypatia.peer2"]
WITH DRIFT MODERATE
SELECT pattern_id FROM scans WHERE severity = "critical";
```

The parser is in-process Elixir; the executors live under `lib/vcl/`:
- `FileExecutor` — single-store local reads
- `RemoteExecutor` — within-org federation
- `CrossOrg` — cross-organisation federation with policy gates

## Watcher / supervision interface

Live monitoring exposed via:

- **TUI** — `mix hypatia.watch` (ANSI dashboard, no deps)
- **HTML** — `http://localhost:9090/` (vanilla JS + EventSource)
- **JSON API** — `/api/status`, `/api/recipes`, `/api/alerts`, `/api/events` (SSE)
- **Prometheus** — `/metrics`
- **GraphQL** — `POST /graphql` (minimal hand-rolled handler)

All `/api/*` endpoints are loopback-only by default; set `HYPATIA_API_BEARER_TOKEN` for cross-host access.

## ABI / FFI

Foreign callers reach Hypatia through:

- **Idris2 ABI** (`src/abi/`) — `Types.idr`, `GraphQL.idr`, `GRPC.idr`, `REST.idr`, `FFI.idr` (with dependent-type proofs)
- **Zig FFI** (`ffi/zig/`) — 7 exported C functions (`hypatia_health_check`, `hypatia_scan_repo`, `hypatia_dispatch`, `hypatia_record_outcome`, `hypatia_force_learning_cycle`, `hypatia_get_confidence`, `hypatia_dispatch_strategy`)

## Component table

| Layer | Tech | Purpose |
|---|---|---|
| Pipeline | Elixir | Rules + dispatch + learning orchestration |
| Neural | Elixir | 8 networks under shared blackboard ETS |
| VCL | Elixir | Query language over verisim-data |
| Safety | Elixir | Rate limiter, quarantine, batch rollback |
| ABI | Idris2 | Typed cross-language interface + proofs |
| FFI | Zig | C ABI bridge |
| CLI / Data | Rust | High-throughput scan workers |
| TUI | Ada 2022 | Optional terminal dashboard |
| Storage | verisim-data | Git-backed canonical flat-file store |

## Detailed architecture references

- [`docs/architecture/togaf-overview.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/togaf-overview.adoc) — TOGAF baseline + ADR-001..ADR-006
- [`docs/architecture/mof-metamodel.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/mof-metamodel.adoc) — OMG MOF M2 metamodel
- [`docs/architecture/NEURAL-ARCHITECTURE.md`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/NEURAL-ARCHITECTURE.md) — neural blackboard details
- [`docs/architecture/boundary-design-options.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/boundary-design-options.adoc) — VCL / SNIF / verisimdb boundary (open ruling: see issue #294)
- [`docs/architecture/topology.md`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/topology.md) — ecosystem topology
- [`docs/architecture/system-integration.md`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/system-integration.md) — how Hypatia integrates with the wider estate

## What changed since the old wiki

- **VQL → VCL** rename (2026-04-05). The query language is now called VCL throughout.
- **ArangoDB → verisim-data**. The graph DB was ditched per the #273 architecture audit; storage is now the git-backed flat-file store.
- **5 networks → 8 networks**. GNN, VAE, and Sequence model joined the blackboard.
- **Hub-and-spoke → blackboard**. Fixed weights replaced with trace-derived confidence.
- **Logtalk → Elixir rules**. The Logtalk rule engine was retired.
