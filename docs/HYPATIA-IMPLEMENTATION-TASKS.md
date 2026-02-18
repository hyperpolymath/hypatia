# Hypatia Implementation Tasks — 2026-02-13

## Priority 1: Learning Loop (CRITICAL)

### Task 1.1: Auto-trigger for OutcomeTracker
**Status:** Learning loop code EXISTS (outcome_tracker.ex 257 lines + learning.lgt 819 lines)
**Gap:** Nothing triggers `update_recipe_confidence/1` after new outcomes arrive
**Fix:** Add GenServer with periodic polling (every 5 min) that:
1. Checks `verisimdb-data/outcomes/` for new JSONL entries since last check
2. Calls `OutcomeTracker.update_recipe_confidence/1` for each recipe with new outcomes
3. Logs confidence changes
**File:** `lib/learning_scheduler.ex` (new)

### Task 1.2: Replace execute_graphql/2 stub
**Status:** fleet_dispatcher.ex line 357-361 — just logs, never dispatches
**Fix:** Replace with real HTTP client calls to gitbot-fleet dispatch-runner.sh
**Approach:** Use `HTTPoison` or `Finch` to POST to fleet webhook endpoint
**Alternative:** If no webhook exists, use file-based dispatch (write to pending.jsonl which dispatch-runner.sh already reads)
**File:** `lib/fleet_dispatcher.ex` lines 357-361

### Task 1.3: File watcher for outcome ingestion
**Fix:** Use `FileSystem` hex package to watch `verisimdb-data/outcomes/` directory
**File:** `lib/outcome_watcher.ex` (new)

## Priority 2: Self-Diagnostics & Fault Tolerance

### Task 2.1: Real health checks
**Status:** health_check() in adapters/src/service.rs and registry/src/Hypatia/API.hs return hardcoded "pass"
**Fix:** Implement actual connectivity checks for ArangoDB, Dragonfly, filesystem
**Files:** `data/src/lib.rs`, `registry/src/Hypatia/API.hs`

### Task 2.2: Self-diagnostic reporting
**Fix:** Add periodic self-check that reports:
- Rule engine health (can Logtalk load and query?)
- Filesystem access (can read/write verisimdb-data, dispatch manifests?)
- Recipe confidence drift (any recipes dropping below threshold?)
- Fleet connectivity (can reach dispatch-runner?)
**File:** `lib/self_diagnostics.ex` (new)

### Task 2.3: Circuit breaker + retry logic
**Fix:** Add circuit breaker pattern for fleet dispatch
- After 3 consecutive failures, open circuit (stop dispatching)
- Auto-retry after cooldown period
- Log all circuit state changes
**File:** `lib/circuit_breaker.ex` (new)

### Task 2.4: Self-healing
**Fix:** When health check fails:
- Attempt auto-recovery (restart connections, clear caches)
- If recovery fails, degrade gracefully (continue scanning, queue dispatches)
- Alert via JSONL log (fleet can pick up)
**File:** `lib/self_healer.ex` (new)

## Priority 3: Neural Networks

### Task 3.1: Radial Basis Function Network
**Purpose:** Pattern similarity detection for code weakness classification
**Implementation:** Elixir module with NIF for compute-heavy RBF kernel operations
**Files:**
- `lib/neural/rbf_network.ex` — Elixir interface
- `native/rbf/src/lib.rs` — Rust NIF for RBF kernel computation
**Architecture:** Input = weakness feature vector, RBF centers = canonical weakness patterns, Output = classification confidence

### Task 3.2: Graph of Trust
**Purpose:** Trust scoring for repos, bots, and fix recipes based on historical outcomes
**Implementation:**
- Vertices: repos, bots, recipes, contributors
- Edges: weighted by outcome success rate
- Trust propagation: PageRank-style scoring
**Files:**
- `lib/neural/graph_of_trust.ex` — Trust graph construction and scoring
- `engine/rules/trust_scoring.lgt` — Logtalk rules for trust-based decisions

### Task 3.3: Mixture of Experts
**Purpose:** Route different weakness types to specialized expert models
**Implementation:**
- Gating network selects expert based on weakness category
- Experts: security, quality, compliance, performance
- Output: weighted combination of expert recommendations
**Files:**
- `lib/neural/mixture_of_experts.ex` — Gating + expert routing
- `lib/neural/expert_security.ex` — Security specialist
- `lib/neural/expert_quality.ex` — Quality specialist
- `lib/neural/expert_compliance.ex` — Compliance specialist

### Task 3.4: Liquid State Machine
**Purpose:** Temporal pattern detection (weakness trends over time)
**Implementation:** Reservoir computing with random sparse connections
**Files:**
- `lib/neural/liquid_state_machine.ex` — LSM implementation
- `native/lsm/src/lib.rs` — Rust NIF for reservoir dynamics

### Task 3.5: Echo State Network
**Purpose:** Time-series prediction of weakness counts and fix success rates
**Implementation:** ESN with leaky integrator neurons
**Files:**
- `lib/neural/echo_state_network.ex` — ESN implementation
- `native/esn/src/lib.rs` — Rust NIF for matrix operations

## Priority 4: API Layer (Idris2-defined)

### Task 4.1: GraphQL API
**Fix:** Wire up Absinthe (Elixir GraphQL) with schema defined from Idris2 ABI
**Files:**
- `src/abi/GraphQL.idr` — Idris2 ABI for GraphQL schema
- `lib/api/schema.ex` — Absinthe schema
- `lib/api/resolvers.ex` — Resolvers calling existing Elixir pipeline
**Depends on:** Phoenix framework addition to mix.exs

### Task 4.2: REST API
**Fix:** Wire up Phoenix REST endpoints matching existing Haskell types
**Files:**
- `lib/api/router.ex` — Phoenix router
- `lib/api/controllers/*.ex` — Controllers for health, scan, dispatch, outcomes

### Task 4.3: gRPC API
**Fix:** Add gRPC service with .proto definitions
**Files:**
- `proto/hypatia.proto` — Service definition
- `lib/api/grpc_server.ex` — gRPC server using grpc-elixir

## Priority 5: Documentation & Integration

### Task 5.1: Enroll all repos in hypatia scans
**Fix:** Update verisimdb-data index.json to include ALL repos (currently 14/585 = 2.4%)
**Script:** scan-all.sh needs to cover all ~248 repos

### Task 5.2: Enroll all repos in gitbot-fleet
**Fix:** Update fleet-coordinator.sh repo list

### Task 5.3: Update all documentation
**Files:** README.adoc, ROADMAP.adoc, STATE.scm, META.scm, ECOSYSTEM.scm, AGENTIC.scm

### Task 5.4: Commit and push all changes
**Fix:** Git add, commit with proper message, push to GitHub and GitLab

---

## Estimated Effort (realistic)

| Priority | Tasks | LOC Estimate | Time |
|----------|-------|-------------|------|
| P1: Learning Loop | 3 tasks | ~400 lines | 30 min |
| P2: Self-Diagnostics | 4 tasks | ~600 lines | 45 min |
| P3: Neural Networks | 5 tasks | ~2000 lines | 2-3 hours |
| P4: API Layer | 3 tasks | ~1500 lines | 1-2 hours |
| P5: Documentation | 4 tasks | ~500 lines | 30 min |

**Total:** ~5000 lines, 5-6 hours

---

## File Dependencies

```
mix.exs needs:
  {:file_system, "~> 1.0"}   # File watching
  {:finch, "~> 0.18"}        # HTTP client
  {:phoenix, "~> 1.7"}       # Web framework (for API)
  {:absinthe, "~> 1.7"}      # GraphQL (for API)
  {:grpc, "~> 0.7"}          # gRPC (for API)
  {:rustler, "~> 0.32"}      # Rust NIFs (for neural networks)

Cargo.toml needs (native/):
  rustler = "0.32"
  ndarray = "0.15"           # Matrix operations for neural networks
```
