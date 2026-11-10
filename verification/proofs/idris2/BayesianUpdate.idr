-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- BayesianUpdate.idr — Bayesian confidence update soundness (H7).
--
-- Re-homed from verification/proofs/lean4/BayesianUpdate.lean (PROOF-POLICY
-- section 3). The statements are the Lean file's; the Lean proofs rested on
-- the kernel axioms propext and Quot.sound, which Idris2 does not have.
--
-- Models the Beta-conjugate update in lib/outcome_tracker.ex
-- (prior_strength = 10):
--   alpha_post = alpha_prior + successes, beta_post = beta_prior + failures,
--   posterior  = alpha_post / (alpha_post + beta_post),
--   result     = clamp(posterior, floor = 10%, cap = 99%).
-- Ratios are compared by cross-multiplication over Nat; Elixir floats are
-- not modelled.

module BayesianUpdate

import Control.Relation
import Data.Nat

%default total

------------------------------------------------------------------------
-- Section 1: fraction comparison over Nat (a/b <= c/d iff a*d <= c*b)
------------------------------------------------------------------------

||| alpha / (alpha + beta) < 1 when beta > 0: alpha < alpha + beta.
export
fracLtOne : (alpha, beta : Nat) -> LT 0 beta -> LT alpha (alpha + beta)
fracLtOne alpha (S b) _ =
  rewrite sym (plusSuccRightSucc alpha b) in LTESucc (lteAddRight alpha)

||| alpha / (alpha + beta) > 0 when alpha > 0.
export
fracPos : (alpha : Nat) -> LT 0 alpha -> LT 0 alpha
fracPos _ h = h

||| The denominator alpha + beta is positive when alpha is.
export
denomPos : (alpha, beta : Nat) -> LT 0 alpha -> LT 0 (alpha + beta)
denomPos alpha beta h = transitive h (lteAddRight alpha)

------------------------------------------------------------------------
-- Section 2: the Beta update
------------------------------------------------------------------------

||| Posterior parameters: alpha = alpha_prior + successes, beta = beta_prior
||| + failures, both positive.
public export
record BayesParams where
  constructor MkBayesParams
  alpha    : Nat
  beta     : Nat
  alphaPos : LT 0 alpha
  betaPos  : LT 0 beta

||| The denominator alpha + beta.
public export
denom : BayesParams -> Nat
denom p = p.alpha + p.beta

||| The denominator is positive.
export
paramsDenomPos : (p : BayesParams) -> LT 0 (denom p)
paramsDenomPos p = denomPos p.alpha p.beta p.alphaPos

------------------------------------------------------------------------
-- Section 3: the posterior is in (0, 1)
------------------------------------------------------------------------

||| H7, property 1: alpha / (alpha + beta) < 1.
export
posteriorLtOne : (p : BayesParams) -> LT p.alpha (denom p)
posteriorLtOne p = fracLtOne p.alpha p.beta p.betaPos

||| H7, property 2: alpha / (alpha + beta) > 0.
export
posteriorPos : (p : BayesParams) -> LT 0 p.alpha
posteriorPos p = p.alphaPos

------------------------------------------------------------------------
-- Section 4: monotonicity
------------------------------------------------------------------------

||| Multiplication by k is monotone.
export
multLteMonotoneLeft : {m, n : Nat} -> (k : Nat) -> LTE m n -> LTE (k * m) (k * n)
multLteMonotoneLeft Z _ = LTEZero
multLteMonotoneLeft (S k) h = plusLteMonotone h (multLteMonotoneLeft k h)

||| H7, property 3: a success weakly raises the ratio:
||| alpha * (alpha + 1 + beta) <= (alpha + 1) * (alpha + beta).
export
moreSuccessesRaises : (p : BayesParams) ->
                      LTE (p.alpha * (p.alpha + 1 + p.beta)) ((p.alpha + 1) * (p.alpha + p.beta))
moreSuccessesRaises p =
  rewrite hs1 in rewrite hs2 in
    plusLteMonotoneLeft (p.alpha * (p.alpha + p.beta)) p.alpha (p.alpha + p.beta)
                        (lteAddRight p.alpha)
  where
    reassoc : p.alpha + 1 + p.beta = (p.alpha + p.beta) + 1
    reassoc = rewrite sym (plusAssociative p.alpha 1 p.beta) in
              rewrite plusCommutative 1 p.beta in
              plusAssociative p.alpha p.beta 1
    hs1 : p.alpha * (p.alpha + 1 + p.beta) = p.alpha * (p.alpha + p.beta) + p.alpha
    hs1 = rewrite reassoc in
          rewrite multDistributesOverPlusRight p.alpha (p.alpha + p.beta) 1 in
          rewrite multOneRightNeutral p.alpha in Refl
    hs2 : (p.alpha + 1) * (p.alpha + p.beta) = p.alpha * (p.alpha + p.beta) + (p.alpha + p.beta)
    hs2 = rewrite multDistributesOverPlusLeft p.alpha 1 (p.alpha + p.beta) in
          rewrite multOneLeftNeutral (p.alpha + p.beta) in Refl

||| H7, property 4: a failure weakly lowers the ratio:
||| alpha * (alpha + beta) <= alpha * (alpha + beta + 1).
export
moreFailuresLowers : (p : BayesParams) ->
                     LTE (p.alpha * (p.alpha + p.beta)) (p.alpha * (p.alpha + p.beta + 1))
moreFailuresLowers p = multLteMonotoneLeft p.alpha (lteAddRight (p.alpha + p.beta))

------------------------------------------------------------------------
-- Section 5: zero observations return the prior
------------------------------------------------------------------------

||| A prior as a fraction (alpha_p = prior * 10, beta_p = (1 - prior) * 10).
public export
record Prior where
  constructor MkPrior
  alphaP    : Nat
  betaP     : Nat
  alphaPPos : LT 0 alphaP
  betaPPos  : LT 0 betaP

||| The posterior after no successes and no failures.
public export
fromPrior : Prior -> BayesParams
fromPrior p = MkBayesParams p.alphaP p.betaP p.alphaPPos p.betaPPos

||| H7, property 5: with no observations the posterior alpha is the prior's.
export
zeroObsAlpha : (p : Prior) -> (fromPrior p).alpha = p.alphaP
zeroObsAlpha _ = Refl

||| H7, property 6: with no observations the posterior beta is the prior's.
export
zeroObsBeta : (p : Prior) -> (fromPrior p).beta = p.betaP
zeroObsBeta _ = Refl

||| H7, property 7: with no observations the denominator is unchanged.
export
zeroObsDenom : (p : Prior) -> denom (fromPrior p) = p.alphaP + p.betaP
zeroObsDenom _ = Refl

------------------------------------------------------------------------
-- Section 6: floor (10%) and cap (99%)
------------------------------------------------------------------------

||| minimum m n never exceeds n.
export
minimumLTERight : (m, n : Nat) -> LTE (minimum m n) n
minimumLTERight Z n = LTEZero
minimumLTERight (S m) Z = LTEZero
minimumLTERight (S m) (S n) = LTESucc (minimumLTERight m n)

||| H7, property 8: the clamped result is at least the floor.
export
clampGeFloor : (raw : Nat) -> LTE 1 (maximum 1 raw)
clampGeFloor raw = maximumLeftUpperBound 1 raw

||| H7, property 9: the clamped result is at most the cap.
export
clampLeCap : (raw : Nat) -> LTE (minimum raw 99) 99
clampLeCap raw = minimumLTERight raw 99

------------------------------------------------------------------------
-- Section 7: composite validity
------------------------------------------------------------------------

||| H7, main theorem: a BayesParams with alpha, beta > 0 satisfies all four
||| validity conditions.
export
bayesianUpdateSound : (p : BayesParams) ->
  ( LT 0 p.alpha
  , LT p.alpha (denom p)
  , LTE (p.alpha * (p.alpha + 1 + p.beta)) ((p.alpha + 1) * (p.alpha + p.beta))
  , LTE (p.alpha * (p.alpha + p.beta)) (p.alpha * (p.alpha + p.beta + 1)) )
bayesianUpdateSound p =
  (posteriorPos p, posteriorLtOne p, moreSuccessesRaises p, moreFailuresLowers p)
