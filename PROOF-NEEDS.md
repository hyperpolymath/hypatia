# PROOF-NEEDS.md — hypatia

## Current State (Updated 2026-04-11)

- **src/abi/*.idr**: `Types.idr`, `FFI.idr`, `GraphQL.idr`, `GRPC.idr`, `REST.idr`, `RuleEngine.idr`
- **verification/proofs/idris2/**: 6 proof files (see below)
- **Dangerous patterns**: 0 in own code (32 references are in rule definitions that detect dangerous patterns in other repos)
- **LOC**: ~144,000 (Rust + Elixir + Idris2)
- **ABI layer**: Comprehensive

## Completed Proofs

| File | Covers | REQUIREMENTS-MASTER.md |
|------|--------|------------------------|
| `verification/proofs/idris2/ConfidenceBounds.idr` | Confidence bounded [0,1]; Bayesian update preserves invariant | H1 ✅ |
| `verification/proofs/idris2/DispatchStrategy.idr` | Dispatch strategy monotone (confidence → strategy mapping) | H2 ✅ |
| `verification/proofs/idris2/SafetyTriangle.idr` | Eliminate > Substitute > Control strict ordering | H3 ✅ |
| `verification/proofs/idris2/Quarantine.idr` | Quarantine trigger exclusivity + release time correctness | H5 ✅ |
| `verification/proofs/idris2/VerisimdbConnector.idr` | VeriSimDB connector data integrity | — |
| `verification/proofs/idris2/BatchRollback.idr` | Batch rollback safety (dispatch reversal correctness) | — |
| `verification/proofs/lean4/RateLimiting.lean` | Window counters never exceed configured bounds; 7 theorems | H4 ✅ |
| `verification/proofs/lean4/BayesianUpdate.lean` | Posterior validity: in (0,1), monotone in successes/failures, zero-obs identity | H7 ✅ |
| `verification/proofs/agda/OutcomeLog.agda` | Timestamps strictly increasing; append preserves MonotoneLog | H6 ✅ |

## What Still Needs Proving

| # | Component | Prover | Priority |
|---|-----------|--------|----------|
| H8 | Kin gate atomicity (repo locks prevent concurrent bot actions) | TLA+ | P1 |
| H9 | Neural consensus aggregation soundness | Agda | P2 |

Note: H8 requires TLA+; H9 requires Agda.

## Recommended Prover

**Idris2** for ABI. **Lean4** for algorithmic properties. **Agda** for structural invariants.

## Priority

**LOW** — H1-H7 complete 2026-04-11. H8 requires TLA+; H9 is P2 Agda.
