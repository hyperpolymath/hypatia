# Proofs Gap Analysis

**Date:** 2026-04-22
**Scope:** VCL query correctness + neural ensemble convergence
**Status:** Inventory + prioritised plan. No code changes yet.

## TL;DR

Hypatia has 14 proof-adjacent files across three active provers (Lean 4,
Idris 2, TLA+) and zero remaining Agda (retired 2026-04 via H6). Only
one prover — TLA+ — is gated in CI. That is the single highest-value
structural gap: eight existing proofs do not stop a breaking change
from landing.

Two concrete correctness smells are worth fixing before proving
anything new:

1. **`lib/neural/graph_of_trust.ex:174`** scales by degree but never
   globally normalises the transition matrix. Combined with the
   negative edge weights the FP penalty path can emit (lines 223-224),
   this can push the matrix spectral radius above 1 and break the
   convergence guarantee PageRank rests on. Proving convergence before
   fixing normalisation would be proving a false claim.
2. **`lib/vcl/client.ex`** query cache uses lazy eviction
   (`now - ts > 2 × TTL`). A stale-read race exists in the window
   between `TTL` and `2×TTL`; a proof of "cache returns equal result
   to uncached execution" would have to either model this explicitly
   or be scoped to exclude the lazy window.

Everything else in this document is forward-looking.

## Current proof inventory

### Lean 4 — `verification/proofs/lean4/`

| File | Proves | CI |
|------|--------|----|
| `BayesianUpdate.lean` | Bayesian confidence update soundness (H7) | ❌ no workflow |
| `RateLimiting.lean` | Sliding-window rate limiter never exceeds configured bound | ❌ no workflow |
| `lakefile.lean` | Lake build config (not a proof) | n/a |

No `sorry`. Gated only by local developer discipline.

### Idris 2 — `verification/proofs/idris2/`

| File | Proves | CI |
|------|--------|----|
| `ConfidenceBounds.idr` | Confidence stays in [0, 1] | ❌ |
| `DispatchStrategy.idr` | confidence → strategy mapping is monotone | ❌ |
| `BatchRollback.idr` | Batch rollback is reversible | ❌ |
| `NeuralConsensus.idr` | Ensemble aggregation is safe | ❌ |
| `SafetyTriangle.idr` | Eliminate > Substitute > Control invariants | ❌ |
| `Quarantine.idr` | Quarantined bots cannot dispatch | ❌ |
| `VerisimdbConnector.idr` | Connector return-type safety | ❌ |

No `believe_me`, no `assert_total`. Pack package definition exists;
CI invocation does not.

### TLA+ — `verification/proofs/tlaplus/`

| File | Proves | CI |
|------|--------|----|
| `KinGate.tla` + `KinGate.cfg` | Kin-gate mutual exclusion (H8) | ✅ `verify-tlaplus.yml` |

This is the one proof currently gated on every push to main and on a
weekly Monday 04:00 UTC schedule. Pattern to replicate.

### Idris 2 type library (not proofs) — `src/abi/`, `verify/src/`

`Types.idr`, `RuleEngine.idr`, `PipelineState.idr` — dependent-type
library code used by the ABI. Type-checked locally; not CI-gated. Not
counted as proofs but will break silently if the Idris toolchain
drifts.

## Runtime-assertion substitutes

| File:line | Invariant | Originally proven? |
|-----------|-----------|--------------------|
| `lib/outcome_tracker.ex:294-306` (`assert_h6_monotone`) | Outcome-log timestamps strictly increase | Yes (Agda, retired 2026-04) |

This is the only documented proof-to-runtime retirement in the tree.
H2-H5 and H9 are referenced nowhere in `lib/`; the H-label scheme is
aspirational from the design docs.

## CI gap — the one big lever

Two missing workflows would promote 9 existing proofs from
"developer-discretion" to "merge-blocking":

- `.github/workflows/verify-lean.yml` — runs `lake build` in
  `verification/proofs/lean4/`, fails on any Lean error.
- `.github/workflows/verify-idris.yml` — runs `pack build` (or
  `idris2 --check` per file) in `verification/proofs/idris2/`, fails
  on type error.

Both can be modelled on the existing `verify-tlaplus.yml`. Estimated
effort: **half-day** (one PR).

This is the single highest-leverage move in this document. Everything
that follows is net-new proofs and assumes these gates exist.

## Properties worth proving — VCL

Ordered by value-per-effort. Values are rough t-shirt sizes assuming
the CI gate is already in place.

### 1. Parser totality — `Easy` (Lean 4, ~1 day)

**Claim:** `Client.parse/1` terminates on every input and produces a
well-typed AST on every well-formed VCL query.

**Why it matters:** The tokenizer already uses a non-backtracking
pipeline (`parse_select → parse_from → parse_where → parse_proof
→ parse_limit → parse_offset`) so the proof reduces to induction on
input length. A malformed-input adversary gets a deterministic
`{:error, ...}` instead of a hang.

**Best prover:** Lean 4 with an inductive grammar definition. No
mathlib dependency.

### 2. Query-result determinism — `Moderate` (Coq, ~3 days)

**Claim:** For a fixed verisim-data snapshot, `FileExecutor.execute/2`
returns identical results across invocations.

**Why it matters:** Filtering, sorting, and pagination already use
pure functions (`Enum.drop/Enum.take`, no randomisation) but the claim
isn't explicit. Federation tags (`_language_family`,
`_drift_discount`) make this stronger than plain list determinism.

**Best prover:** Coq — operational semantics of the execute loop,
induction on result stream. Lean mathlib can also reach it.

### 3. Cache coherence — `Moderate` (TLA+, ~3 days)

**Claim:** If `RemoteCache.cache_remote_store/2` returns `{:ok,
path}`, then the clone at `path` is ≤TTL seconds old.

**Why it matters:** The lazy-eviction window (see TL;DR) is exactly
the kind of concurrency gap TLA+ catches. Model:
`{Initial, Cloned, Pulled, Stale, Evicted}` states with temporal
property `□(hit ⟹ age ≤ TTL)`. Likely finds the lazy-window bug and
forces either eager eviction or an explicit exclusion.

**Best prover:** TLA+ — pattern already established by KinGate.

### 4. Federation consistency — `Hard` (Idris 2, ~2 weeks)

**Claim:** Every cross-repo result tagged with `_language_family = F`
comes from a repo in family `F`, and every `_drift_discount = d` is
the policy-computed discount for `F` under the active drift policy.

**Why it matters:** Federation is the most recently added surface and
the most fragile. Dependent types let the proof piggy-back on the
existing type structure rather than a separate semantic model.

**Best prover:** Idris 2 — `Result language_family drift_discount` as
a dependent pair.

### 5. WHERE-clause totality — `Research-grade` (Coq, ~1 month)

**Claim:** For any item and WHERE condition, `apply_where` terminates
and returns a filtered list.

**Why it matters:** Regex depth and nested field access are the only
real termination risks. Worth doing only after regex inputs become an
adversary surface (federation-inbound queries from untrusted
sources). Skip for now.

## Properties worth proving — Neural

### 1. PageRank power-iteration convergence — `Easy` (Lean 4, ~1 day) — **blocked by bug fix**

**Claim:** For any non-negative weight matrix W with ρ(W) ≤ 1, the
damped iteration x_{k+1} = (1-α)/n + α·W·x_k converges to a unique
fixed point regardless of initialisation.

**Why this can't be proven today:** The bug in
`graph_of_trust.ex:174` means the "non-negative, ρ ≤ 1" premise
doesn't hold. Fix normalisation first, then the proof is almost
verbatim mathlib.

### 2. ESN spectral-radius stability — `Moderate` (Lean 4, ~3 days)

**Claim:** If reservoir matrix R is scaled to ρ(R) < 1, the state
trajectory x_k = R·x_{k-1} + u_k stays bounded for any bounded input
sequence and exponentially forgets its initial state.

**Why it matters:** The scaling at
`lib/neural/echo_state_network.ex:197-200` already enforces the
premise. Proof closes the loop between enforcement and the claim the
enforcement is meant to license.

**Best prover:** Lean 4 + `analysis.normed_space.exponential_summable`.

### 3. K-means convergence to local minimum — `Moderate` (Lean 4, ~3 days)

**Claim:** The k-means objective is non-increasing across iterations
and converges in finite time.

**Why it matters:** RBF kernel centre selection
(`lib/neural/radial_neural_network.ex:197`) relies on k-means; the
max-50-iteration cap and 0.001 threshold make this a finite check but
not a proof of progress.

### 4. RBF universal approximation — `Hard` (Lean 4, ~3 weeks)

**Claim:** Gaussian RBF networks with finite centres approximate any
continuous function on a compact domain to within ε.

**Why it matters:** This is the math that justifies using RBF at all.
Lovely to have. Not load-bearing; training-pipeline convergence
proofs are more urgent.

### 5. MoE load-balancing convergence — `Research-grade` (Lean 4, ~2 months)

**Claim:** With auxiliary load-balancing loss, expert activation
distribution converges to an ε-neighbourhood of uniform.

**Why it matters:** Dynamical-systems proof. Interesting. Skip for
now — the 0.01 auxiliary weight at
`lib/neural/mixture_of_experts.ex:28` is so small this is almost
decorative.

## Other gaps the scouts surfaced

- **LSM separation property** — `lib/neural/liquid_state_machine.ex`
  assumes random connections give separation; never validated. Not a
  proof target, a test target (property-based).
- **ESN ridge regression via gradient descent** — 
  `lib/neural/echo_state_network.ex` uses GD instead of closed-form.
  Not a convergence guarantee. Either switch to closed-form or add a
  convergence test.
- **Coordinator ensemble joint convergence** — the weighted sum at
  `lib/neural/coordinator.ex:520-529` is ad-hoc. No joint proof
  possible until the aggregation rule is principled.

## Recommended sequencing

**PR 1 — CI gate (half-day).** `verify-lean.yml` + `verify-idris.yml`.
Promotes 9 existing proofs from discretionary to mandatory. Biggest
ROI in the document. **✅ Landed 2026-04-22 via `claude/quality-gates`.**

**PR 2 — Fix graph_of_trust normalisation (half-day).** Then PR 3
becomes available. **✅ Landed 2026-04-22 via `claude/quality-gates`.**

**PR 3 — Prove VCL parser totality (1 day).** Easy win, unblocks
other parser-adjacent proofs. **✅ Landed 2026-04-22 via
`claude/lean-proofs-parser-pagerank` —
`verification/proofs/lean4/ParserTotality.lean`.**

**PR 4 — Prove PageRank convergence (1 day).** Net-new mathematical
assurance over the neural layer, only after PR 2. **⚠ Partially
landed 2026-04-22 via `claude/lean-proofs-parser-pagerank` —
preconditions only (non-negativity, column-stochastic scaling
premises) in `PageRankInvariants.lean`. Full analytical convergence
requires Mathlib; still open.**

**PR 5 — Prove ESN stability (3 days).** Same shape as PR 4.
**⚠ Partially landed (awaiting merge) via `claude/esn-spectral-scaling`
— `ESNSpectralScaling.lean` proves scaling preconditions
(well-defined denominator, floor-clamping). Full analytical echo-state
property also blocked on Mathlib.**

**PR 6+ — Pick from:** query-result determinism, cache coherence,
k-means convergence, federation consistency. Each is its own branch.

## Out of scope

- Proof of VeriSimDB graph/temporal/vector modality invariants —
  these are upstream, not Hypatia's perimeter.
- Proof of training-pipeline weight updates — needs Nx/EXLA backend
  first (see ROADMAP).
- Proofs against an adversarial drift-policy executor — depends on
  VeriSimDB federation executor being live.

## One-line status

**2026-04-22 update:** the half-day CI PR landed; Lean and Idris
proofs are now merge-blocking. 11 proof files gated in CI (2 original
Lean + 3 new Lean + 7 Idris — awaiting one Idris stamp cycle).
Remaining tractable work is mathlib integration for the analytical
halves of PageRank + ESN convergence.
