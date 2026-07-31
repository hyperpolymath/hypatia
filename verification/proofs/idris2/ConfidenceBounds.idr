-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- ConfidenceBounds.idr — Formal proof that confidence values are always in [0, 1]
--
-- Models confidence as a refined natural-number fraction (numerator/denominator)
-- with denominator > 0. All operations (creation, Bayesian update, clamping,
-- weighted average) are proven to preserve the invariant:
--   0 <= numerator <= denominator
--
-- This avoids floating-point reasoning entirely; the Elixir runtime uses
-- Double but the invariant holds structurally over the rational model.
--
-- Corresponds to:
--   - lib/outcome_tracker.ex        (Bayesian confidence updates)
--   - lib/confidence_annealing.ex   (clamping, floor/cap)
--   - src/abi/Types.idr             (Confidence record)

module ConfidenceBounds

import Data.Nat

%default total

------------------------------------------------------------------------
-- Core type: bounded confidence as a fraction
------------------------------------------------------------------------

||| A confidence value represented as a fraction num/den where:
|||   - den > 0      (denominator is positive)
|||   - num <= den   (fraction is at most 1)
||| Together with num being Nat (>= 0), this gives 0 <= num/den <= 1.
public export
record BoundedConfidence where
  constructor MkBounded
  num : Nat
  den : Nat
  {auto denPositive : LTE 1 den}
  {auto numBounded  : LTE num den}

||| Zero confidence: 0/1
public export
zeroConfidence : BoundedConfidence
zeroConfidence = MkBounded 0 1

||| Full confidence: 1/1
public export
fullConfidence : BoundedConfidence
fullConfidence = MkBounded 1 1

||| Construct confidence from a fraction, returning Nothing if invalid.
public export
mkConfidence : (n : Nat) -> (d : Nat) -> {auto dp : LTE 1 d} -> {auto nb : LTE n d} -> BoundedConfidence
mkConfidence n d = MkBounded n d

------------------------------------------------------------------------
-- Proof: construction preserves bounds
------------------------------------------------------------------------

||| The numerator of any BoundedConfidence is <= its denominator.
||| This is the core [0, 1] invariant — witnessed by the numBounded field.
public export
numLeqDen : (c : BoundedConfidence) -> LTE c.num c.den
numLeqDen c = c.numBounded

||| The denominator of any BoundedConfidence is >= 1.
public export
denPositive : (c : BoundedConfidence) -> LTE 1 c.den
denPositive c = c.denPositive

------------------------------------------------------------------------
-- Bayesian update model (Beta distribution)
------------------------------------------------------------------------

||| Bayesian state: alpha successes, beta failures.
||| Confidence = alpha / (alpha + beta).
||| Invariant: alpha + beta >= 1 (at least one observation or prior).
public export
record BayesConfidence where
  constructor MkBayes
  alpha : Nat
  beta  : Nat
  {auto nonEmpty : LTE 1 (alpha + beta)}

||| Convert Bayesian state to a BoundedConfidence.
||| alpha / (alpha + beta) is always in [0, 1] because alpha <= alpha + beta.
public export
bayesToBounded : BayesConfidence -> BoundedConfidence
bayesToBounded (MkBayes a b @{ne}) =
  MkBounded a (a + b) {denPositive = ne} {numBounded = lteAddRight a}

||| Record a success: increment alpha.
||| Proof that the result is still valid (alpha + 1 + beta >= 1).
public export
recordSuccess : BayesConfidence -> BayesConfidence
recordSuccess (MkBayes a b @{ne}) =
  MkBayes (S a) b @{succLemma ne}
  where
    succLemma : LTE 1 (a + b) -> LTE 1 (S a + b)
    succLemma _ = LTESucc LTEZero

||| Record a failure: increment beta.
||| Proof that the result is still valid (alpha + beta + 1 >= 1).
public export
recordFailure : BayesConfidence -> BayesConfidence
recordFailure (MkBayes a b @{ne}) =
  MkBayes a (S b) @{failLemma a b}
  where
    failLemma : (a : Nat) -> (b : Nat) -> LTE 1 (a + S b)
    failLemma Z b = LTESucc LTEZero
    failLemma (S a') b = LTESucc LTEZero

------------------------------------------------------------------------
-- Proof: success preserves [0, 1] bounds
------------------------------------------------------------------------

||| After recording a success, the confidence fraction is still in [0, 1].
||| Specifically: (alpha + 1) <= (alpha + 1 + beta).
public export
successPreservesBounds : (bc : BayesConfidence)
                      -> let bc' = recordSuccess bc
                         in LTE bc'.alpha (bc'.alpha + bc'.beta)
successPreservesBounds (MkBayes a b) = lteAddRight (S a)

------------------------------------------------------------------------
-- Proof: failure preserves [0, 1] bounds
------------------------------------------------------------------------

||| After recording a failure, the confidence fraction is still in [0, 1].
||| Specifically: alpha <= (alpha + beta + 1).
public export
failurePreservesBounds : (bc : BayesConfidence)
                      -> let bc' = recordFailure bc
                         in LTE bc'.alpha (bc'.alpha + bc'.beta)
failurePreservesBounds (MkBayes a b) = alphaLeqSum a (S b)
  where
    alphaLeqSum : (a : Nat) -> (b : Nat) -> LTE a (a + b)
    alphaLeqSum Z _ = LTEZero
    alphaLeqSum (S a') b = LTESucc (alphaLeqSum a' b)

------------------------------------------------------------------------
-- Confidence floor and cap (clamping)
------------------------------------------------------------------------

||| Clamp a confidence value to [floor, cap] where floor <= cap.
||| Models ConfidenceAnnealing floor=0.10, cap=0.99 as naturals:
|||   floor = 10/100, cap = 99/100.
||| We work with a common denominator for simplicity.
public export
clampNat : (val : Nat) -> (floor : Nat) -> (cap : Nat) -> (den : Nat)
        -> {auto floorLeqCap : LTE floor cap}
        -> {auto capLeqDen : LTE cap den}
        -> (n : Nat ** LTE n den)
clampNat val floor cap den =
  if leNat val floor
    then (floor ** transitive floorLeqCap capLeqDen)
    else if leNat cap val
      then (cap ** capLeqDen)
      else case isLTE val den of
        Yes prf => (val ** prf)
        No contra => (cap ** capLeqDen)  -- unreachable when val < cap <= den
  where
    leNat : (a : Nat) -> (b : Nat) -> Bool
    leNat Z _ = True
    leNat (S _) Z = False
    leNat (S a) (S b) = leNat a b

||| Proof: clamped value is always <= denominator (i.e., <= 1).
public export
clampUpperBound : (val, floor, cap, den : Nat)
               -> {auto flc : LTE floor cap}
               -> {auto cld : LTE cap den}
               -> LTE (fst (clampNat val floor cap den)) den
clampUpperBound val floor cap den = snd (clampNat val floor cap den)

------------------------------------------------------------------------
-- Confidence monotonicity under success
------------------------------------------------------------------------

||| Cross-multiplication proof that success is non-decreasing:
|||   a / (a + b) <= (a+1) / (a+1+b)
||| Equivalent to: a * (a + 1 + b) <= (a + 1) * (a + b)
||| Expanding: a^2 + a + ab <= a^2 + ab + a + b
||| Simplifies to: 0 <= b (always true for Nat).
public export
successNonDecreasing : (a : Nat) -> (b : Nat)
                    -> LTE (a * (S a + b)) (S a * (a + b))
-- a * (S a + b) = a * S(a + b) = a + a*(a+b)  [multRightSuccPlus]
-- S a * (a + b) = (a + b) + a*(a+b)            [def of *]
-- so the goal reduces to a + a*(a+b) <= (a+b) + a*(a+b), i.e. a <= a + b.
successNonDecreasing a b =
  rewrite multRightSuccPlus a (a + b) in
    plusLteMonotoneRight (a * (a + b)) a (a + b) (lteAddRight a)

------------------------------------------------------------------------
-- Confidence monotonicity under failure
------------------------------------------------------------------------

||| Cross-multiplication proof that failure is non-increasing:
|||   a / (a + b + 1) <= a / (a + b)
||| Equivalent to: a * (a + b) <= a * (a + b + 1)
||| Which is: a * (a + b) <= a * (a + b) + a
||| I.e., 0 <= a (always true for Nat).
public export
failureNonIncreasing : (a : Nat) -> (b : Nat)
                    -> LTE (a * (a + b)) (a * (a + S b))
-- a * (a + S b) = a * S(a + b) = a + a*(a+b)  [multRightSuccPlus]
-- so the goal a*(a+b) <= a + a*(a+b) is a <= addend on the left.
failureNonIncreasing a b =
  rewrite sym (plusSuccRightSucc a b) in     -- a + S b => S (a + b)
  rewrite multRightSuccPlus a (a + b) in     -- a * S(a + b) => a + a*(a+b)
  rewrite plusCommutative a (a * (a + b)) in
    lteAddRight (a * (a + b))

------------------------------------------------------------------------
-- Weighted average preserves bounds
------------------------------------------------------------------------

||| When combining two bounded confidences with weights, the result
||| stays in [0, 1]. This models the neural coordinator's weighted
||| aggregation of multiple network predictions.
|||
||| (w1 * n1 * d2 + w2 * n2 * d1) / ((w1 + w2) * d1 * d2)
|||
||| We prove the simpler case: average of two fractions.
||| (n1 * d2 + n2 * d1) / (2 * d1 * d2) <= 1
||| i.e., n1 * d2 + n2 * d1 <= 2 * d1 * d2
|||
||| Since n1 <= d1 and n2 <= d2:
|||   n1 * d2 <= d1 * d2  and  n2 * d1 <= d2 * d1
|||   so n1 * d2 + n2 * d1 <= d1 * d2 + d2 * d1 = 2 * d1 * d2

||| Helper: if a <= b then a * c <= b * c.
public export
multLteMonoRight : (a, b, c : Nat) -> LTE a b -> LTE (a * c) (b * c)
multLteMonoRight Z b c _ = LTEZero
multLteMonoRight (S a') (S b') c (LTESucc prf) =
  -- S a' * c = c + a'*c, S b' * c = c + b'*c; add c on the left of the IH.
  plusLteMonotoneLeft c (a' * c) (b' * c) (multLteMonoRight a' b' c prf)

||| Proof: sum of cross-products bounded by twice the product of denominators.
||| Given n1 <= d1 and n2 <= d2:
|||   n1 * d2 + n2 * d1 <= d1 * d2 + d1 * d2
public export
averageBounded : (n1, d1, n2, d2 : Nat)
              -> LTE n1 d1 -> LTE n2 d2
              -> LTE (n1 * d2 + n2 * d1) (d1 * d2 + d1 * d2)
averageBounded n1 d1 n2 d2 prf1 prf2 =
  let left  : LTE (n1 * d2) (d1 * d2)
      left  = multLteMonoRight n1 d1 d2 prf1
      -- n2 * d1 <= d2 * d1, rewritten to d1 * d2 via commutativity.
      right : LTE (n2 * d1) (d1 * d2)
      right = rewrite multCommutative d1 d2 in multLteMonoRight n2 d2 d1 prf2
  in plusLteMonotone left right
