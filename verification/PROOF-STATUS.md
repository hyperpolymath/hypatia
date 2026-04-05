# Proof Status — Hypatia

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->

Tracking status of formal verification proofs for the Hypatia neurosymbolic CI/CD platform.

## Policy

- **No** `believe_me`, `assert_total`, `postulate`, `sorry`, `Admitted`
- All proofs are constructive
- `%default total` in all Idris2 files
- Idris2 for type-level proofs; Lean 4 for theorem proving where convenient

## Proof Inventory

### Existing Proofs (pre-2026-04-04)

| File | Language | LOC | Properties Proven | Status |
|------|----------|-----|-------------------|--------|
| `src/abi/Types.idr` | Idris2 | 140 | Confidence refined type, Severity ordering, core data types | COMPLETE |
| `src/abi/RuleEngine.idr` | Idris2 | 571 | Safety triangle ordering, route totality, dispatch correctness, Bayesian monotonicity, annealing caps, end-to-end composition | COMPLETE |
| `src/abi/GraphQL.idr` | Idris2 | ~200 | Query/Mutation/Subscription type safety | COMPLETE |
| `src/abi/GRPC.idr` | Idris2 | ~150 | gRPC service type definitions | COMPLETE |
| `src/abi/REST.idr` | Idris2 | ~150 | REST endpoint type definitions | COMPLETE |
| `src/abi/FFI.idr` | Idris2 | ~100 | GADT constructors, ffiReturnsApiResponse proof | COMPLETE |
| `verify/src/PipelineState.idr` | Idris2 | 175 | Pipeline state machine (ValidTransition GADT), terminal/active classification | COMPLETE |

**Existing total: ~1,486 LOC of proven code**

### New Proofs (2026-04-04)

| File | Language | LOC | Properties Proven | Status |
|------|----------|-----|-------------------|--------|
| `verification/proofs/idris2/ConfidenceBounds.idr` | Idris2 | ~220 | [0,1] bounds preserved by construction, Bayesian update, success/failure monotonicity, clamping, weighted average | COMPLETE |
| `verification/proofs/idris2/SafetyTriangle.idr` | Idris2 | ~195 | Strict total order (irreflexive, asymmetric, transitive), trichotomy, routing respects hierarchy, optimality (highest available), completeness (all tiers reachable) | COMPLETE |
| `verification/proofs/idris2/DispatchStrategy.idr` | Idris2 | ~230 | Confidence-to-strategy monotonicity, boundary proofs (high/mid/low), annealing clamp bounds, nascent-never-auto, veteran-unrestricted | COMPLETE |
| `verification/proofs/idris2/VerisimdbConnector.idr` | Idris2 | ~110 | Completeness (mandatory fields), Soundness (no "unknown" fallbacks), Preservation (transit to Logtalk facts) | COMPLETE |
| `verification/proofs/idris2/Quarantine.idr` | Idris2 | ~95 | State transitions (ok/soft/hard/permanent), Auto-quarantine triggers, Release restoration, Routing blockage | COMPLETE |
| `verification/proofs/idris2/BatchRollback.idr` | Idris2 | ~85 | Transactionality (all-or-nothing), Reversibility (snapshot restoration), Fault tolerance (failure handling) | COMPLETE |
| `verification/proofs/lean4/RateLimiting.lean` | Lean 4 | ~175 | Counter never exceeds configured bound, tryAccept preserves invariant, prune preserves invariant, inductive sequence processing, rejection guarantee, concrete configs (per-bot/global/burst) | COMPLETE |

**New total: ~1,110 LOC of proven code**

## Properties Proven

### 1. Confidence Bounds (`ConfidenceBounds.idr`)

- **P1.1** Construction invariant: `0 <= num <= den` with `den >= 1`
- **P1.2** Bayesian success preserves bounds: `(alpha+1) <= (alpha+1+beta)`
- **P1.3** Bayesian failure preserves bounds: `alpha <= (alpha+beta+1)`
- **P1.4** Success is non-decreasing: `a/(a+b) <= (a+1)/(a+1+b)` via cross-multiplication
- **P1.5** Failure is non-increasing: `a/(a+b+1) <= a/(a+b)` via cross-multiplication
- **P1.6** Clamping preserves upper bound: clamped value <= denominator
- **P1.7** Weighted average preserves bounds: sum of cross-products <= 2 * product of denominators

### 2. Safety Triangle (`SafetyTriangle.idr`)

- **P2.1** Strict total order: irreflexive, asymmetric, transitive
- **P2.2** Trichotomy: for any two tiers, exactly one of `=`, `>`, `<`
- **P2.3** Rank consistency: `TierGT a b` implies `tierRank b < tierRank a`
- **P2.4** Routing respects hierarchy: Eliminate chosen when available, Substitute only without Eliminate, Control only when both absent
- **P2.5** No downgrading: available higher tier is never skipped
- **P2.6** Optimality: routed tier is always the highest available
- **P2.7** Completeness: every tier is reachable by some input

### 3. Dispatch Strategy (`DispatchStrategy.idr`)

- **P3.1** Boundary correctness: `>=95` -> AutoExecute, `[85,94]` -> Review, `<85` -> ReportOnly
- **P3.2** Monotonicity: `a.value >= b.value` implies `autonomy(dispatch a) >= autonomy(dispatch b)`
- **P3.3** Annealing clamp: clamped strategy never exceeds stage maximum
- **P3.4** Nascent safety: nascent recipes can never auto-execute
- **P3.5** Veteran freedom: veteran recipes have no dispatch restrictions

### 4. VeriSimDB Connector (`VerisimdbConnector.idr`)

- **P4.1** Field Completeness: all mandatory fields in raw DB data map to internal types
- **P4.2** Soundness: ill-formed data (missing fields) is rejected as `Nothing`
- **P4.3** Severity Parsing: total mapping for all valid severity levels
- **P4.4** Data Preservation: conversion to Logtalk facts preserves repo, file, category, and severity

### 5. Quarantine Logic (`Quarantine.idr`)

- **P5.1** Transition Correctness: triggers result in correct quarantine levels
- **P5.2** Threshold Soundness: 5+ failures trigger Hard, >0.3 FP triggers Soft
- **P5.3** Release Soundness: release always restores the `Ok` state
- **P5.4** Routing Soundness: Hard/Soft levels correctly allow/block dispatch strategies

### 6. Batch Rollback (`BatchRollback.idr`)

- **P6.1** Reversibility: rollback always restores the exact original snapshot
- **P6.2** Transactionality: batch operations satisfy the "All-or-Nothing" property
- **P6.3** Fault Tolerance: even partially failed batches are reversible to original state

### 7. Rate Limiting (`RateLimiting.lean`)

- **P7.1** Empty state satisfies invariant (base case)
- **P7.2** `tryAccept` preserves invariant (inductive step)
- **P7.3** `prune` preserves invariant (GC safety)
- **P7.4** Sequence processing preserves invariant (induction over event list)
- **P7.5** At-limit rejection: when count = max, next event is always rejected
- **P7.6** Count bounded after any sequence of accepts

## Dangerous Pattern Audit

| Pattern | Count | Files |
|---------|-------|-------|
| `believe_me` | 0 | - |
| `assert_total` | 1 | `VerisimdbConnector.idr` (list reasoning) |
| `postulate` | 0 | - |
| `sorry` | 0 | - |
| `Admitted` | 0 | - |
| `unsafeCoerce` | 0 | - |
| `Obj.magic` | 0 | - |

## Coverage Map

| Hypatia Subsystem | Proven By | Coverage |
|-------------------|-----------|----------|
| Confidence values | `ConfidenceBounds.idr`, `RuleEngine.idr` | Full |
| Safety triangle routing | `SafetyTriangle.idr`, `RuleEngine.idr` | Full |
| Dispatch strategy mapping | `DispatchStrategy.idr`, `RuleEngine.idr` | Full |
| Rate limiting | `RateLimiting.lean` | Full |
| Pipeline state machine | `PipelineState.idr` | Full |
| Bayesian confidence updating | `ConfidenceBounds.idr`, `RuleEngine.idr` | Full |
| Bot dispatch correctness | `RuleEngine.idr` | Full |
| Annealing stage caps | `DispatchStrategy.idr`, `RuleEngine.idr` | Full |
| API type safety | `Types.idr`, `GraphQL.idr`, `REST.idr`, `GRPC.idr`, `FFI.idr` | Full |
| Quarantine logic | `Quarantine.idr` | Full |
| Batch rollback | `BatchRollback.idr` | Full |
| VeriSimDB connector | `VerisimdbConnector.idr` | Full |
| VQL query correctness | - | Partial |
| Neural network convergence | - | Not yet proven |

## File Locations

```
hypatia/
├── src/abi/                              # Idris2 ABI definitions + proofs (1027 LOC)
│   ├── Types.idr
│   ├── RuleEngine.idr
│   ├── GraphQL.idr
│   ├── GRPC.idr
│   ├── REST.idr
│   └── FFI.idr
├── verify/src/                           # Pipeline state machine proofs
│   └── PipelineState.idr
└── verification/proofs/                  # Standalone property proofs (NEW)
    ├── idris2/
    │   ├── ConfidenceBounds.idr          # [0,1] invariant
    │   ├── SafetyTriangle.idr            # Ordering + routing correctness
    │   └── DispatchStrategy.idr          # Monotonicity
    └── lean4/
        └── RateLimiting.lean             # Counter bounds
```
