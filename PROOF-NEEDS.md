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

## What Still Needs Proving

| # | Component | Prover | Priority |
|---|-----------|--------|----------|
| H4 | Rate limit enforcement (window counters never exceed bounds) | L4 | P0 |
| H6 | Outcome log monotonicity (timestamps strictly increasing) | Agda | P1 |
| H7 | Bayesian confidence update soundness (posterior validity) | L4 | P1 |
| H8 | Kin gate atomicity (repo locks prevent concurrent bot actions) | TLA+ | P1 |
| H9 | Neural consensus aggregation soundness | Agda | P2 |

Note: H4/H7 require Lean4; H6/H9 require Agda; H8 requires TLA+. Not actionable in this I2 sweep.

## Recommended Prover

**Idris2** — Already in use for H1-H5. Remaining items require different provers.

## Priority

**LOW** (was HIGH) — H1, H2, H3, H5 complete. H4/H6/H7/H8/H9 require L4/Agda/TLA+ — different prover specialists needed.
