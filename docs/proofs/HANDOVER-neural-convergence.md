<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# HANDOVER — Complete the proof corpus: neural-convergence proofs (Lean 4 + Mathlib)

## Mission
Hypatia's formal-proof corpus spans **Idris 2, Lean 4, and TLA+**. Every existing
proof verifies and is CI-gated. **One obligation is open: neural-network
convergence.** Two Lean files prove only ℕ *preconditions*; the real analytical
theorems (PageRank power-iteration convergence; ESN echo-state stability) need
**Mathlib**. This must be done in an environment where Mathlib's `.olean` cache
CDN is reachable (the Claude-on-the-web sandbox blocks it — `lakecache.blob.core.windows.net`
and `mathlib4.lean-cache.cloud` 403). Locally, with normal internet, it just works.

## Repo state (do not redo)
- Branch: `claude/hopeful-ptolemy-ZdYqp` (proof work is merged to `main`).
- #433 — made the corpus actually compile (it had never been machine-checked).
- #434 — CI hardening: gate the Idris ABI + verify packages, pin Lean, fix the Idris cache.
- #440 — reconciled the proof-status docs.
- Pinned toolchain: **Idris 2 `v0.7.0`**, **Lean `v4.30.0`**, TLA+ tla2tools 2.19.
  Agda retired. Zero escape hatches (`believe_me`/`assert_total`/`postulate`/`sorry`/`Admitted`/`unsafeCoerce` = 0).

## The corpus (current truth)
| Obligation | File | Status |
|---|---|---|
| H1 confidence bounds | `verification/proofs/idris2/ConfidenceBounds.idr` | ✅ |
| H2 dispatch monotonicity | `verification/proofs/idris2/DispatchStrategy.idr` | ✅ |
| H3 safety-triangle order | `verification/proofs/idris2/SafetyTriangle.idr` | ✅ |
| H4 rate limiting | `verification/proofs/lean4/RateLimiting.lean` | ✅ |
| H5 quarantine | `verification/proofs/idris2/Quarantine.idr` | ✅ |
| H6 outcome-log monotonicity | `lib/outcome_tracker.ex` (runtime assertion; Agda retired) | ✅ |
| H7 Bayesian update | `verification/proofs/lean4/BayesianUpdate.lean` | ✅ |
| H8 kin-gate atomicity | `verification/proofs/tlaplus/KinGate.tla` | ✅ |
| H9 neural consensus aggregation | `verification/proofs/idris2/NeuralConsensus.idr` | ✅ |
| parser totality | `verification/proofs/lean4/ParserTotality.lean` | ✅ |
| ABI package + verify package | `src/abi/*.idr` (incl. `RuleEngine.idr`), `verify/src/*.idr` | ✅ |
| **Neural convergence — PageRank** | `verification/proofs/lean4/PageRankInvariants.lean` | ⛔ preconditions only |
| **Neural convergence — ESN** | `verification/proofs/lean4/ESNSpectralScaling.lean` | ⛔ preconditions only |

## Toolchain install (local)
```bash
curl -fL https://raw.githubusercontent.com/leanprover/elan/v4.0.0/elan-init.sh | sh -s -- -y
elan toolchain install leanprover/lean4:v4.30.0     # matches the repo pin
# Idris2 v0.7.0 (only to re-verify the Idris side): chezscheme + bootstrap, or idris2-pack
# TLA+ (only to re-verify H8): Java 21 + tla2tools.jar
```

## CI-hygiene design decision (read before coding)
Do **not** add Mathlib to the existing `verification/proofs/lean4/` project — it is
the *fast* gate (no deps, builds in seconds, runs every push). Create a **separate**
project `verification/proofs/lean4-mathlib/` with its own `lakefile.lean`
(`require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "v4.30.0"`),
its own `lean-toolchain` (`leanprover/lean4:v4.30.0`), and the new proof files. Gate it
in a **separate cached CI job** (below). Keep the existing ℕ-precondition files where they are.

## Theorem 1 — PageRank power-iteration convergence
- New file `PageRankConvergence.lean` in the mathlib project. Models `lib/neural/graph_of_trust.ex`.
- **Statement:** for `xₖ₊₁ = α·P·xₖ + (1-α)·v` with `P` column-stochastic and `0 ≤ α < 1`,
  there is a unique fixed point and the iterates converge to it.
- **Strategy:** the damped Google operator `G(x) = α·P·x + (1-α)·v` is a contraction
  with modulus `α` in the ℓ¹ norm (column-stochastic ⇒ `‖P x‖₁ ≤ ‖x‖₁`). Then Banach.
- **Mathlib:** `ContractingWith` (`Mathlib.Topology.MetricSpace.Contracting`),
  `ContractingWith.fixedPoint`, `…fixedPoint_unique`, `…tendsto_iterate_fixedPoint`.
  Space `EuclideanSpace ℝ (Fin n)` or `PiLp 1`; matrices via `Matrix.toLin'`. (Confirm exact
  lemma names in v4.30.0.)

## Theorem 2 — ESN echo-state stability
- New file `ESNStability.lean`. Models `lib/neural/echo_state_network.ex`.
- **⚠️ Frame as operator-norm contraction (`‖W‖ < 1`), NOT spectral radius `ρ(W) < 1`.**
  The runtime scales by the **max-abs entry** (bounds an operator/elementwise norm); it does
  **not** compute the spectral radius. The operator-norm statement is code-faithful and far
  more tractable in Mathlib.
- **Statement:** if `‖W‖ < 1`, the reservoir map `x ↦ W·x + u` is a contraction ⇒ unique
  bounded trajectory for any bounded input + exponential forgetting of the initial state.
- **Mathlib:** same `ContractingWith` machinery; `LipschitzWith`, `ContinuousLinearMap` operator
  norm; `ContractingWith.dist_fixedPoint_fixedPoint`-style bounds for the forgetting rate.

## Hard constraints
- No `sorry`/`believe_me`/`postulate`/`assert_total`/`admit`/`native_decide`. Fully constructive.
- SPDX header on every file (`MPL-2.0` + the copyright line above).
- Lean stays pinned at v4.30.0; Mathlib is the `v4.30.0` tag (exists, matches).
- When done: flip both rows PARTIAL→COMPLETE in `verification/PROOF-STATUS.md` and `docs/proofs/needs.md`.

## CI job to add (separate, cached — parallel to `lake-build`, not folded in)
```yaml
  lake-build-mathlib:
    name: lake build (mathlib)
    runs-on: ubuntu-latest
    timeout-minutes: 60                # `lake exe cache get` is ~5 GB
    env: { LEAN_TOOLCHAIN: leanprover/lean4:v4.30.0 }
    steps:
      - uses: actions/checkout@<sha>
      - # install elan + Lean v4.30.0 (mirror the existing lake-build job)
      - uses: actions/cache@<sha>
        with:
          path: |
            ~/.elan
            ~/.cache/mathlib
            verification/proofs/lean4-mathlib/.lake
          key: mathlib-v4.30.0-${{ runner.os }}-${{ hashFiles('verification/proofs/lean4-mathlib/**/*.lean') }}
      - run: cd verification/proofs/lean4-mathlib && lake exe cache get && lake build
```
Add `verification/proofs/lean4-mathlib/**` to the workflow `paths:` (push + pull_request).

## Verify the whole corpus before finishing
```bash
idris2 --build src/abi/hypatia-abi.ipkg
idris2 --build verify/hypatia-verify.ipkg
cd verification/proofs/idris2 && for f in *.idr; do idris2 --check "$f" || exit 1; done; cd -
cd verification/proofs/lean4 && lake build && cd -
cd verification/proofs/lean4-mathlib && lake exe cache get && lake build && cd -
cd verification/proofs/tlaplus && java -cp /path/tla2tools.jar tlc2.TLC -config KinGate.cfg KinGate.tla
```

## Gotchas already solved (recognise these in the codebase)
- base-0.7 renames: `lteRefl`→`reflexive`, `lteTransitive`→`transitive`; Nat lemmas
  `multRightSuccPlus`, `plusLteMonotone*`, `lteAddRight`.
- Lean 4.30: `List.mem_cons_self` has implicit args; `List.filter_cons`, `decide_eq_true_eq`;
  use `induction … generalizing` when the IH must hold for a changing state.
- Idris refutation of impossible equalities / empty GADTs: `Refl impossible` /
  `case x of _ impossible` (no `Uninhabited` instance to lean on).
- Idris reserved words used as identifiers were the bug class: `data`, `record`, `partial`.
- `src/abi/*.idr` are the real files; `src/Hypatia/ABI/*.idr` are **symlinks** into them
  (build namespace `Hypatia.ABI.*`).

## Definition of done
1. Both theorems proven in Lean+Mathlib, no escape hatches, `lake build` clean.
2. Separate cached CI job gating them; workflow `paths:` updated.
3. `PROOF-STATUS.md` + `needs.md` rows flipped to COMPLETE.
4. Whole-corpus verification passes.
5. Draft PR against `main`.

## Out of scope (other contributors)
Repo-hygiene PR (173 medium scan findings: `timeout-minutes` on `ci.yml`/`clusterfuzzlite.yml`,
the `setup-java` SHA-pin false-positive); `PipelineState`→real `proven` integration; documenting
the `src/abi` symlink layout; the `gitbot-fleet` / `.git-private-farm` repos.
