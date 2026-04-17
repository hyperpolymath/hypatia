-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- NeuralConsensus.idr - Formal proof that the ensemble coordinator's
-- aggregation of n neural-network predictions yields a confidence in [0, 1],
-- and that the result is bounded below by the ensemble minimum.
--
-- H9 in standards/docs/proofs/spec-templates/T1-critical/hypatia.md.
--
-- Corresponds to:
--   - lib/neural/coordinator.ex  (aggregate_predictions/1)
--   - 8 contributing networks: graph_of_trust, mixture_of_experts,
--     liquid_state_machine, echo_state_network, radial_neural_network,
--     plus 3 auxiliary signals (novelty gate, quarantine state, trust prior).
--
-- Model: each network emits an integer in [0, scale]. scale is typically
-- 10_000 so a 4-decimal-place confidence Double round-trips faithfully.
-- Aggregation is uniform mean: sum_of_values / (n * scale).
-- We prove the numerator is bounded by the denominator (i.e. result in [0, 1]),
-- and that the numerator is at least n times the minimum input value.

module NeuralConsensus

import Data.Nat
import Data.Vect

%default total

------------------------------------------------------------------------
-- Core model
------------------------------------------------------------------------

||| A single network's output: a natural in [0, scale].
||| The bounded invariant is carried in the type so that once constructed,
||| a Prediction cannot escape the valid range.
public export
record Prediction (scale : Nat) where
  constructor MkPred
  value : Nat
  {auto valueBounded : LTE value scale}

||| An ensemble of n networks at a shared scale.
||| The canonical Hypatia coordinator uses n = 8.
public export
Ensemble : (n : Nat) -> (scale : Nat) -> Type
Ensemble n scale = Vect n (Prediction scale)

||| The Hypatia 8-network ensemble.
public export
HypatiaEnsemble : (scale : Nat) -> Type
HypatiaEnsemble = Ensemble 8

------------------------------------------------------------------------
-- Basic Nat lemmas used throughout
------------------------------------------------------------------------

||| Reflexivity of LTE on Nat.
lteReflNat : (n : Nat) -> LTE n n
lteReflNat Z = LTEZero
lteReflNat (S n') = LTESucc (lteReflNat n')

||| A <= b + A, growing by adding b on the left.
lteAddLeftAny : {b : Nat} -> (a : Nat) -> LTE a (b + a)
lteAddLeftAny {b = Z} a = lteReflNat a
lteAddLeftAny {b = S b'} a = lteSuccRight (lteAddLeftAny {b = b'} a)

||| If b <= d then b <= c + d, for any c.
lteGrowLeft : {c : Nat} -> LTE b d -> LTE b (c + d)
lteGrowLeft {c = Z} prf = prf
lteGrowLeft {c = S c'} prf = lteSuccRight (lteGrowLeft {c = c'} prf)

||| Monotonicity of addition on both sides.
plusLteMonoBoth : {c : Nat} -> LTE a c -> LTE b d -> LTE (a + b) (c + d)
plusLteMonoBoth LTEZero q = lteGrowLeft q
plusLteMonoBoth (LTESucc p) q = LTESucc (plusLteMonoBoth p q)

||| min on Nat, computable.
natMin : Nat -> Nat -> Nat
natMin Z _ = Z
natMin (S _) Z = Z
natMin (S a) (S b) = S (natMin a b)

||| natMin is a lower bound on the first argument.
minLteLeft : (a, b : Nat) -> LTE (natMin a b) a
minLteLeft Z _ = LTEZero
minLteLeft (S _) Z = LTEZero
minLteLeft (S a) (S b) = LTESucc (minLteLeft a b)

||| natMin is a lower bound on the second argument.
minLteRight : (a, b : Nat) -> LTE (natMin a b) b
minLteRight Z _ = LTEZero
minLteRight (S _) Z = LTEZero
minLteRight (S a) (S b) = LTESucc (minLteRight a b)

------------------------------------------------------------------------
-- Aggregation: uniform mean
------------------------------------------------------------------------

||| Sum the values of an ensemble.
public export
sumPreds : {n, scale : Nat} -> Ensemble n scale -> Nat
sumPreds [] = 0
sumPreds (p :: ps) = p.value + sumPreds ps

||| Aggregated confidence as a fraction (num, den).
||| den = n * scale so the fraction is the uniform mean of the inputs.
public export
aggregate : {n, scale : Nat} -> Ensemble n scale -> (Nat, Nat)
aggregate {n} {scale} preds = (sumPreds preds, n * scale)

||| Minimum of a non-empty ensemble.
public export
minPred : {n, scale : Nat} -> Ensemble (S n) scale -> Nat
minPred {n = Z} (MkPred v :: []) = v
minPred {n = S n'} (MkPred v :: (q :: qs)) = natMin v (minPred {n = n'} (q :: qs))

------------------------------------------------------------------------
-- Main theorem: aggregate produces a value in [0, 1]
------------------------------------------------------------------------

||| Core lemma: the ensemble's numerator is bounded by the denominator.
|||   sumPreds preds <= n * scale
||| Equivalently, the aggregated mean is in [0, 1].
public export
sumPredsBounded : {n, scale : Nat}
               -> (preds : Ensemble n scale)
               -> LTE (sumPreds preds) (n * scale)
sumPredsBounded {n = Z} [] = LTEZero
sumPredsBounded {n = S n'} {scale} (MkPred v @{vb} :: ps) =
  -- goal: LTE (v + sumPreds ps) (scale + n' * scale)
  plusLteMonoBoth vb (sumPredsBounded ps)

||| Main H9 theorem: the aggregated confidence is in [0, 1].
||| Stated as: numerator <= denominator.
public export
aggregateInBounds : {n, scale : Nat}
                 -> (preds : Ensemble n scale)
                 -> let (num, den) = aggregate preds in LTE num den
aggregateInBounds preds = sumPredsBounded preds

||| Specialization to the canonical 8-network ensemble.
public export
hypatiaAggregateInBounds : {scale : Nat}
                        -> (preds : HypatiaEnsemble scale)
                        -> let (num, den) = aggregate preds in LTE num den
hypatiaAggregateInBounds = aggregateInBounds

------------------------------------------------------------------------
-- Extension: aggregate is bounded below by n * min(inputs)
------------------------------------------------------------------------

||| The sum of predictions is at least (S n) * minPred for a non-empty ensemble.
||| Proof by induction on the ensemble. This is the uniform-weight case of the
||| classical "weighted average is bounded below by the minimum input" result.
public export
sumPredsGeMin : {n, scale : Nat}
             -> (preds : Ensemble (S n) scale)
             -> LTE ((S n) * (minPred preds)) (sumPreds preds)
sumPredsGeMin {n = Z} (MkPred v :: []) =
  -- goal: LTE (v + (0 + 0)) (v + 0)
  -- Both sides reduce to v; use Nat reflexivity after computation.
  lteReflNat (v + 0)
sumPredsGeMin {n = S n'} {scale} (MkPred v :: (q :: qs)) =
  let m : Nat
      m = natMin v (minPred (q :: qs))
      -- m <= v and m <= minPred (q :: qs)
      mLeV : LTE m v
      mLeV = minLteLeft v (minPred (q :: qs))
      mLeRest : LTE m (minPred (q :: qs))
      mLeRest = minLteRight v (minPred (q :: qs))
      -- inductive hypothesis: (S n') * minPred (q :: qs) <= sumPreds (q :: qs)
      ih : LTE ((S n') * (minPred (q :: qs))) (sumPreds (q :: qs))
      ih = sumPredsGeMin (q :: qs)
      -- (S n') * m <= (S n') * minPred (q :: qs) (by mLeRest)
      step1 : LTE ((S n') * m) ((S n') * (minPred (q :: qs)))
      step1 = multLteMonoLeft (S n') m (minPred (q :: qs)) mLeRest
      -- chain: (S n') * m <= sumPreds (q :: qs)
      step2 : LTE ((S n') * m) (sumPreds (q :: qs))
      step2 = transitive step1 ih
      -- goal becomes: LTE (m + (S n') * m) (v + sumPreds (q :: qs))
  in plusLteMonoBoth mLeV step2
  where
    multLteMonoLeft : (k, a, b : Nat) -> LTE a b -> LTE (k * a) (k * b)
    multLteMonoLeft Z _ _ _ = LTEZero
    multLteMonoLeft (S k') a b prf =
      plusLteMonoBoth prf (multLteMonoLeft k' a b prf)

------------------------------------------------------------------------
-- Aggregation is well-defined (denominator positive) for non-empty ensembles
------------------------------------------------------------------------

||| For a non-empty ensemble with scale >= 1, the denominator is at least 1.
||| This is the well-formedness companion to aggregateInBounds: together they
||| say aggregate preds is a valid (num, den) pair with den > 0 and num <= den.
public export
aggregateDenPositive : {n, scale : Nat}
                    -> {auto scalePos : LTE 1 scale}
                    -> (preds : Ensemble (S n) scale)
                    -> let (_, den) = aggregate preds in LTE 1 den
aggregateDenPositive {n} {scale} {scalePos} _ =
  -- den = (S n) * scale, and scale >= 1 so den >= (S n) >= 1.
  case scale of
    S scale' =>
      -- (S n) * (S scale') = S n + n * (S scale')
      -- starts with S, so >= 1.
      LTESucc LTEZero
    Z => absurd (succNotLTEzero scalePos)
