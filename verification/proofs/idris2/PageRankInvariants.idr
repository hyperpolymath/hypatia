-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- PageRankInvariants.idr — preconditions of the PageRank iteration, over Nat.
--
-- Re-homed from verification/proofs/lean4/PageRankInvariants.lean
-- (PROOF-POLICY section 3: these are engineering preconditions, so they live
-- in Idris2). The statements are the Lean file's.
--
-- lib/neural/graph_of_trust.ex (build_edges/1) keeps edge weights
-- non-negative, divides each source's contributions by its outgoing-weight
-- sum, and keeps the damping factor in [0, 1]. This file states the first
-- two over a Nat model. Convergence of the iteration is a theory theorem,
-- planned in Agda over constructive reals
-- (docs/proofs/HANDOVER-neural-convergence.adoc); it is not proved here.

module PageRankInvariants

import Data.List
import Data.Nat

%default total

------------------------------------------------------------------------
-- Section 1: the model
------------------------------------------------------------------------

||| A weighted directed edge; weights are absolute counts on a shared scale.
public export
record Edge where
  constructor MkEdge
  src    : Nat
  tgt    : Nat
  weight : Nat

||| The sum of the weights of the edges leaving `src`.
public export
outWeightSum : List Edge -> (src : Nat) -> Nat
outWeightSum edges s = foldl (\acc, e => acc + e.weight) 0 (filter (\e => e.src == s) edges)

||| The sum of the weights of the edges entering `tgt`.
public export
inWeightSum : List Edge -> (tgt : Nat) -> Nat
inWeightSum edges t = foldl (\acc, e => acc + e.weight) 0 (filter (\e => e.tgt == t) edges)

------------------------------------------------------------------------
-- Section 2: non-negativity
------------------------------------------------------------------------

||| Property 1: edge weights are non-negative.
export
edgeWeightNonneg : (e : Edge) -> LTE 0 e.weight
edgeWeightNonneg _ = LTEZero

||| Property 2: every outgoing-weight sum is non-negative.
export
outWeightSumNonneg : (edges : List Edge) -> (s : Nat) -> LTE 0 (outWeightSum edges s)
outWeightSumNonneg _ _ = LTEZero

||| Property 3: every incoming-weight sum is non-negative.
export
inWeightSumNonneg : (edges : List Edge) -> (t : Nat) -> LTE 0 (inWeightSum edges t)
inWeightSumNonneg _ _ = LTEZero

------------------------------------------------------------------------
-- Section 3: the empty graph
------------------------------------------------------------------------

||| Property 4: with no edges, every outgoing-weight sum is 0.
export
outWeightSumNil : (s : Nat) -> outWeightSum [] s = 0
outWeightSumNil _ = Refl

||| Property 5: with no edges, every incoming-weight sum is 0.
export
inWeightSumNil : (t : Nat) -> inWeightSum [] t = 0
inWeightSumNil _ = Refl

------------------------------------------------------------------------
-- Section 4: composite
------------------------------------------------------------------------

||| Main theorem: the step preconditions build_edges/1 maintains hold for
||| every graph.
export
pagerankPreconditionsSound : (edges : List Edge) -> (e : Edge) -> (t : Nat) ->
  (LTE 0 e.weight, LTE 0 (outWeightSum edges e.src), LTE 0 (inWeightSum edges t))
pagerankPreconditionsSound edges e t =
  (edgeWeightNonneg e, outWeightSumNonneg edges e.src, inWeightSumNonneg edges t)
