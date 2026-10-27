-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- ESNSpectralScaling.lean — Structural preconditions for ESN echo-state
-- stability, proven over ℕ.
--
-- PROOFS-GAP.md: Neural Property 2 — ESN spectral-radius stability.
--
-- Context: `lib/neural/echo_state_network.ex` scales the reservoir
-- matrix so its induced infinity-norm (the largest absolute row sum) is
-- a configured `@reservoir_norm` (default 0.95):
--
--     max_row_sum = matrix |> map(row -> sum(map(row, abs))) |> max(1.0)
--     scale       = @reservoir_norm / max(max_row_sum, 0.001)
--     scaled      = Enum.map(matrix, fn row -> Enum.map(row, &(&1 * scale)) end)
--
-- That scaling is the runtime enforcement of the echo-state property.
-- With ‖R‖∞ < 1, leak rate a and tanh (1-Lipschitz), the update
-- `x ↦ (1 - a)·x + a·tanh(R·x + w·u)` is a contraction in the sup norm
-- with modulus `1 - a + a·‖R‖∞`, so the state trajectory forgets its
-- initial state exponentially. (Until 2026-10 the runtime scaled the
-- largest single entry instead, which does not bound ‖R‖∞: the 80-node
-- reservoir had a spectral radius near 2 and the property failed.) The
-- analytical half of that statement needs Mathlib — out of scope for
-- the Mathlib-free proof set (see the header note in
-- BayesianUpdate.lean).
--
-- What this file proves, over ℕ, is that the **scaling operation is
-- well-defined**: the floor-guarded `max(max_row_sum, floor)`
-- denominator is strictly positive whenever the floor is, which is the
-- load-bearing precondition for avoiding divide-by-zero in the
-- runtime. The Mathlib follow-up is tracked in
-- docs/proofs/HANDOVER-neural-convergence.adoc.

------------------------------------------------------------------------
-- Section 1: The model
--
-- Reservoir entries are ℕ (scaled integer magnitudes; negatives and
-- rationals would require Mathlib). The norm is the max over rows of
-- each row's sum of entry magnitudes. Scaling is integer division by
-- the ratio (norm / target).
------------------------------------------------------------------------

/-- Matrix as a list of rows. -/
def Matrix := List (List Nat)

/-- Each row's sum of entry magnitudes. Mirrors
    `Enum.map(row, &abs/1) |> Enum.sum()` in Elixir. -/
def rowSums (m : Matrix) : List Nat := m.map List.sum

/-- Pointwise max of a list of ℕ with a provided floor for the empty
    case. Mirrors `Enum.max(fn -> 1.0 end)` in Elixir. -/
def maxAbsOr (xs : List Nat) (floor : Nat) : Nat :=
  xs.foldl (fun acc x => if x > acc then x else acc) floor

/-- Induced infinity-norm (largest row sum) of a matrix, with a floor. -/
def matrixMaxAbs (m : Matrix) (floor : Nat) : Nat :=
  maxAbsOr (rowSums m) floor

/-- Scaling ratio of target to the norm under a positive floor.
    Models `scale = reservoir_norm / max(max_row_sum, 0.001)` in Elixir. -/
def scaleRatio (target maxAbs floor : Nat) : Nat :=
  target / (max maxAbs floor)

------------------------------------------------------------------------
-- Section 2: Empty-matrix base cases
------------------------------------------------------------------------

/-- ESN — PROPERTY 1: An empty matrix has no row sums. -/
theorem rowSums_nil : rowSums [] = [] := rfl

/-- ESN — PROPERTY 2: `maxAbsOr` on an empty list returns the floor. -/
theorem maxAbsOr_nil (floor : Nat) : maxAbsOr [] floor = floor := rfl

/-- ESN — PROPERTY 3: An empty matrix has norm equal to its floor. -/
theorem matrixMaxAbs_nil (floor : Nat) : matrixMaxAbs [] floor = floor := rfl

------------------------------------------------------------------------
-- Section 3: Non-negativity — ℕ-trivial but load-bearing downstream
------------------------------------------------------------------------

/-- ESN — PROPERTY 4: The norm is non-negative for any matrix. -/
theorem matrixMaxAbs_nonneg (m : Matrix) (floor : Nat) :
    0 ≤ matrixMaxAbs m floor := Nat.zero_le _

/-- ESN — PROPERTY 5: The scaling ratio itself is non-negative. -/
theorem scaleRatio_nonneg (target maxAbs floor : Nat) :
    0 ≤ scaleRatio target maxAbs floor := Nat.zero_le _

------------------------------------------------------------------------
-- Section 4: The denominator is strictly positive
--
-- The runtime uses `max(max_row_sum, 0.001)` precisely to keep the
-- denominator above zero. In ℕ land the positive floor is any
-- non-zero constant. This is the load-bearing precondition: without
-- it the scaling operation would divide by zero at runtime.
------------------------------------------------------------------------

/-- ESN — PROPERTY 6: When the floor is strictly positive, so is the
    floor-clamped max. This guarantees the scaling ratio's denominator
    is non-zero. -/
theorem denom_pos_when_floor_pos (maxAbs floor : Nat) (hf : 0 < floor) :
    0 < max maxAbs floor :=
  Nat.lt_of_lt_of_le hf (Nat.le_max_right maxAbs floor)

/-- ESN — PROPERTY 7: If the raw `maxAbs` happens to be positive, the
    clamped denominator is at least `maxAbs`. Lets downstream proofs
    reason about the ratio without relying on the floor. -/
theorem denom_ge_maxAbs (maxAbs floor : Nat) :
    maxAbs ≤ max maxAbs floor := Nat.le_max_left maxAbs floor

/-- ESN — PROPERTY 8: The clamped denominator is at least the floor.
    Symmetric to PROPERTY 7; completes the "max is an upper-bound of
    both" lemma. -/
theorem denom_ge_floor (maxAbs floor : Nat) :
    floor ≤ max maxAbs floor := Nat.le_max_right maxAbs floor

------------------------------------------------------------------------
-- Section 5: Composite soundness theorem
------------------------------------------------------------------------

/-- ESN — MAIN THEOREM: The scaling preconditions that
    `lib/neural/echo_state_network.ex` `generate_reservoir/1` enforces
    are structurally sound.

    For any reservoir matrix and any strictly-positive floor:

      1. Max-abs is non-negative (ℕ-trivial).
      2. The floor-clamped denominator is strictly positive, so the
         scaling ratio is well-defined.
      3. The clamped denominator is bounded below by both `max_row_sum`
         and `floor`.

    These are the preconditions a Mathlib-based echo-state-property
    proof (trajectory boundedness + exponential forgetting) would
    take as hypotheses. The analytical half is deferred until
    Mathlib joins the Lean proof set — tracked as PR 5 in
    PROOFS-GAP.md. -/
theorem esn_scaling_preconditions_sound
    (m : Matrix) (maxAbs floor : Nat) (hf : 0 < floor) :
    (0 ≤ matrixMaxAbs m floor) ∧
    (0 < max maxAbs floor) ∧
    (maxAbs ≤ max maxAbs floor) ∧
    (floor ≤ max maxAbs floor) :=
  ⟨matrixMaxAbs_nonneg m floor,
   denom_pos_when_floor_pos maxAbs floor hf,
   denom_ge_maxAbs maxAbs floor,
   denom_ge_floor maxAbs floor⟩
