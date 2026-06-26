<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

# PROOF-NEEDS.md — hypatia

## Current State (Updated 2026-06-05)

- **src/abi/*.idr**: `Types.idr`, `FFI.idr`, `GraphQL.idr`, `GRPC.idr`,
  `REST.idr`, `RuleEngine.idr` — built as the `hypatia-abi` package under `--total`.
- **verify/src/*.idr**: `PipelineState.idr`, `Verify/Fuel.idr` — `hypatia-verify` package.
- **verification/proofs/idris2/**: 7 standalone proof files (`idris2 --check`).
- **verification/proofs/lean4/**: 5 Lean files (`lake build`).
- **verification/proofs/tlaplus/**: `KinGate.tla` (TLC model-check).
- **Toolchain**: Idris2 `v0.7.0`, Lean `v4.30.0` (both pinned), TLA+ tla2tools 2.19.
  Agda was retired 2026-04-22 (see PROOF-STATUS.md "Retired proofs").
- **Escape hatches**: 0 — no `believe_me` / `assert_total` / `postulate` /
  `sorry` / `Admitted` / `unsafeCoerce` in any proof file.
- All of the above is gated by `.github/workflows/verify-proofs.yml`.

## Completed Proofs

| File | Covers | Obligation |
|------|--------|------------|
| `verification/proofs/idris2/ConfidenceBounds.idr` | Confidence bounded [0,1]; Bayesian update preserves invariant | H1 ✅ |
| `verification/proofs/idris2/DispatchStrategy.idr` | Dispatch strategy monotone (confidence → strategy) | H2 ✅ |
| `verification/proofs/idris2/SafetyTriangle.idr` | Eliminate > Substitute > Control strict ordering | H3 ✅ |
| `verification/proofs/lean4/RateLimiting.lean` | Window counters never exceed configured bounds | H4 ✅ |
| `verification/proofs/idris2/Quarantine.idr` | Quarantine trigger exclusivity + release correctness | H5 ✅ |
| `lib/outcome_tracker.ex` (`assert_h6_monotone/2`) | Outcome-log timestamp monotonicity — runtime assertion + echidnabot audit | H6 (retired from Agda) |
| `verification/proofs/lean4/BayesianUpdate.lean` | Posterior in (0,1), monotone in successes/failures, zero-obs identity | H7 ✅ |
| `verification/proofs/tlaplus/KinGate.tla` | Kin-gate atomicity / mutual exclusion (TLC: 1.78M states, no errors) | H8 ✅ |
| `verification/proofs/idris2/NeuralConsensus.idr` | Uniform-mean aggregation in [0,1], bounded below by n·min | H9 ✅ |
| `verification/proofs/idris2/VerisimdbConnector.idr` | Connector completeness / soundness / preservation | — |
| `verification/proofs/idris2/BatchRollback.idr` | Batch rollback transactionality / reversibility | — |
| `verification/proofs/lean4/ParserTotality.lean` | VCL `FROM`-clause parser totality + round-trip | — |

## What Still Needs Proving

These are the **only** open proof obligations. Both currently prove ℕ
*structural preconditions* over the natural-number model; the full analytical
theorems require Mathlib and are in progress.

| Component | Proven today | Target theorem | Prover |
|-----------|--------------|----------------|--------|
| PageRank power-iteration convergence (`PageRankInvariants.lean`) | non-negativity, column-stochastic normalisation preconditions | Banach-contraction convergence of the damped Google matrix (α < 1) | Lean 4 + Mathlib |
| ESN echo-state stability (`ESNSpectralScaling.lean`) | floor-clamped denominator is positive (scaling well-defined) | operator-norm contraction ⇒ bounded trajectory + exponential forgetting | Lean 4 + Mathlib |

## Recommended Prover

**Idris2** for ABI / type-level invariants. **Lean 4** for algorithmic and
numerical properties (**+ Mathlib** for the analytic convergence results).
**TLA+** for model-checked concurrency. (Agda is no longer in the toolchain.)

## Priority

**MEDIUM** — H1–H9 are complete (H6 as a runtime assertion). The remaining work
is the two neural-convergence theorems, which require pulling in Mathlib.