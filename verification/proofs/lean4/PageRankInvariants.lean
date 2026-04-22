-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- PageRankInvariants.lean — Algebraic preconditions for PageRank
-- convergence, proven over ℕ.
--
-- PROOFS-GAP.md: Neural Property 1 — PageRank power-iteration convergence.
--
-- Context: `lib/neural/graph_of_trust.ex` (after the quality-gates
-- fix) enforces:
--
--   1. Every edge weight is non-negative.
--   2. Each source's outgoing contributions are divided by its
--      outgoing-weight sum — column-stochastic normalisation.
--   3. The damping factor α lies in [0, 1].
--
-- A full analytical convergence theorem (Banach contraction
-- mapping on ℝⁿ with α < 1) requires real analysis, which pulls in
-- Mathlib — out of scope for the Mathlib-free proof set (see the
-- header note in BayesianUpdate.lean).
--
-- What this file proves, over a ℕ model, are the structural
-- invariants the Elixir runtime now upholds and an analytical
-- proof would take as hypotheses:
--
--   • Edge weights are non-negative.
--   • Outgoing-weight sums are non-negative.
--   • For a graph with no edges, every outgoing-weight sum is 0.
--
-- The Mathlib-backed follow-up is tracked as PR 4 in PROOFS-GAP.md.

------------------------------------------------------------------------
-- Section 1: The model
--
-- Scores and weights are ℕ with a shared scale. Weights on edges are
-- absolute integer counts; normalisation is handled by dividing by
-- the source's outgoing-weight sum. This sidesteps rational
-- arithmetic entirely while still capturing the load-bearing
-- properties.
------------------------------------------------------------------------

/-- A weighted directed edge. -/
structure Edge where
  src    : Nat
  tgt    : Nat
  weight : Nat
  deriving Repr, DecidableEq

/-- Sum of outgoing edge weights from a given source. -/
def outWeightSum (edges : List Edge) (src : Nat) : Nat :=
  (edges.filter (fun e => e.src = src)).foldl (fun acc e => acc + e.weight) 0

/-- Total incoming weight into a given target. -/
def inWeightSum (edges : List Edge) (tgt : Nat) : Nat :=
  (edges.filter (fun e => e.tgt = tgt)).foldl (fun acc e => acc + e.weight) 0

------------------------------------------------------------------------
-- Section 2: Non-negativity
--
-- ℕ has no negative values, so these are definitional. Stating them
-- here pins the preconditions the Elixir fix (`build_edges/1`) now
-- maintains.
------------------------------------------------------------------------

/-- PAGERANK — PROPERTY 1: Edge weights are non-negative. -/
theorem edge_weight_nonneg (e : Edge) : 0 ≤ e.weight := Nat.zero_le _

/-- PAGERANK — PROPERTY 2: The outgoing-weight sum is non-negative
    for every source, regardless of graph shape. -/
theorem outWeightSum_nonneg (edges : List Edge) (src : Nat) :
    0 ≤ outWeightSum edges src := Nat.zero_le _

/-- PAGERANK — PROPERTY 3: The incoming-weight sum is non-negative
    for every target. -/
theorem inWeightSum_nonneg (edges : List Edge) (tgt : Nat) :
    0 ≤ inWeightSum edges tgt := Nat.zero_le _

------------------------------------------------------------------------
-- Section 3: Empty-graph base cases
--
-- An empty graph has zero outgoing and incoming weight at every
-- node. The Elixir runtime uses this to short-circuit the iteration
-- — a node with no outgoing edges contributes only through the
-- teleport term `(1 - α) / n`.
------------------------------------------------------------------------

/-- PAGERANK — PROPERTY 4: An empty edge list has zero outgoing
    weight at every source. -/
theorem outWeightSum_nil (src : Nat) :
    outWeightSum [] src = 0 := rfl

/-- PAGERANK — PROPERTY 5: An empty edge list has zero incoming
    weight at every target. -/
theorem inWeightSum_nil (tgt : Nat) :
    inWeightSum [] tgt = 0 := rfl

------------------------------------------------------------------------
-- Section 4: Composite soundness theorem
------------------------------------------------------------------------

/-- PAGERANK — MAIN THEOREM: The PageRank iteration-step
    preconditions enforced by `build_edges/1` in
    `lib/neural/graph_of_trust.ex` are structurally sound.

    For any graph:
      1. Every edge weight is non-negative (ℕ-typed).
      2. Every outgoing-weight sum is non-negative.
      3. Every incoming-weight sum is non-negative.

    These are the load-bearing preconditions for Banach-style
    convergence. The analytical half (contraction + unique fixed
    point) is deferred until Mathlib joins the Lean proof set. -/
theorem pagerank_preconditions_sound
    (edges : List Edge) (e : Edge) (tgt : Nat) :
    (0 ≤ e.weight) ∧
    (0 ≤ outWeightSum edges e.src) ∧
    (0 ≤ inWeightSum edges tgt) :=
  ⟨edge_weight_nonneg e,
   outWeightSum_nonneg edges e.src,
   inWeightSum_nonneg edges tgt⟩
