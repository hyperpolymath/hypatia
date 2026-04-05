-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- DispatchStrategy.idr — Formal proof of confidence-to-strategy monotonicity
--
-- Models the dispatch strategy mapping:
--   >= 95% confidence -> AutoExecute  (highest autonomy)
--   85%-94%           -> Review       (medium autonomy)
--   < 85%             -> ReportOnly   (lowest autonomy)
--
-- Proves that higher confidence always produces equal or higher autonomy
-- (monotonicity), and that the strategy boundaries are respected.
--
-- Uses natural number encoding (confidence as percentage 0-100) to avoid
-- floating-point reasoning while capturing the same invariants.
--
-- Corresponds to:
--   - lib/triangle_router.ex        (dispatch_strategy/1)
--   - lib/confidence_annealing.ex   (clamp_strategy/2, max_dispatch_tier/1)
--   - src/abi/RuleEngine.idr        (dispatchStrategy, clampStrategy)

module DispatchStrategy

import Data.Nat

%default total

------------------------------------------------------------------------
-- Section 1: Strategy type and ordering
------------------------------------------------------------------------

||| Dispatch strategies, ordered by autonomy level.
public export
data Strategy = ReportOnly | Review | AutoExecute

||| Numeric autonomy level: higher = more autonomous.
public export
autonomyLevel : Strategy -> Nat
autonomyLevel ReportOnly  = 0
autonomyLevel Review      = 1
autonomyLevel AutoExecute = 2

||| Strict ordering on strategies.
public export
data StrategyGT : Strategy -> Strategy -> Type where
  AutoGtReview  : StrategyGT AutoExecute Review
  AutoGtReport  : StrategyGT AutoExecute ReportOnly
  ReviewGtReport : StrategyGT Review ReportOnly

||| Proof: strategy ordering is irreflexive.
public export
strategyIrreflexive : StrategyGT s s -> Void
strategyIrreflexive AutoGtReview impossible
strategyIrreflexive AutoGtReport impossible
strategyIrreflexive ReviewGtReport impossible

||| Proof: strategy ordering is transitive.
public export
strategyTransitive : StrategyGT a b -> StrategyGT b c -> StrategyGT a c
strategyTransitive AutoGtReview ReviewGtReport = AutoGtReport

||| Decidable equality for Strategy.
public export
DecEq Strategy where
  decEq ReportOnly  ReportOnly  = Yes Refl
  decEq ReportOnly  Review      = No (\case Refl impossible)
  decEq ReportOnly  AutoExecute = No (\case Refl impossible)
  decEq Review      ReportOnly  = No (\case Refl impossible)
  decEq Review      Review      = Yes Refl
  decEq Review      AutoExecute = No (\case Refl impossible)
  decEq AutoExecute ReportOnly  = No (\case Refl impossible)
  decEq AutoExecute Review      = No (\case Refl impossible)
  decEq AutoExecute AutoExecute = Yes Refl

------------------------------------------------------------------------
-- Section 2: Confidence as bounded natural (percentage 0-100)
------------------------------------------------------------------------

||| A confidence percentage: a natural number in [0, 100].
public export
record ConfidencePct where
  constructor MkPct
  value : Nat
  {auto bounded : LTE value 100}

||| Threshold constants (matching Elixir: 0.95 -> 95, 0.85 -> 85).
public export
autoThreshold : Nat
autoThreshold = 95

public export
reviewThreshold : Nat
reviewThreshold = 85

------------------------------------------------------------------------
-- Section 3: Dispatch function (total, deterministic)
------------------------------------------------------------------------

||| Map a confidence percentage to a dispatch strategy.
||| This is the formal model of TriangleRouter.dispatch_strategy/1.
public export
dispatch : ConfidencePct -> Strategy
dispatch (MkPct v) =
  case isLTE 95 v of
    Yes _ => AutoExecute
    No _  => case isLTE 85 v of
      Yes _ => Review
      No _  => ReportOnly

------------------------------------------------------------------------
-- Section 4: Boundary proofs
------------------------------------------------------------------------

||| Proof: confidence >= 95 always yields AutoExecute.
public export
highConfidenceIsAuto : (c : ConfidencePct) -> LTE 95 c.value
                    -> dispatch c = AutoExecute
highConfidenceIsAuto (MkPct v) prf with (isLTE 95 v)
  highConfidenceIsAuto (MkPct v) prf | Yes _ = Refl
  highConfidenceIsAuto (MkPct v) prf | No contra = absurd (contra prf)

||| Proof: confidence in [85, 94] always yields Review.
public export
midConfidenceIsReview : (c : ConfidencePct) -> LTE 85 c.value
                     -> Not (LTE 95 c.value)
                     -> dispatch c = Review
midConfidenceIsReview (MkPct v) ge85 not95 with (isLTE 95 v)
  midConfidenceIsReview (MkPct v) ge85 not95 | Yes prf = absurd (not95 prf)
  midConfidenceIsReview (MkPct v) ge85 not95 | No _ with (isLTE 85 v)
    midConfidenceIsReview (MkPct v) ge85 not95 | No _ | Yes _ = Refl
    midConfidenceIsReview (MkPct v) ge85 not95 | No _ | No contra = absurd (contra ge85)

||| Proof: confidence < 85 always yields ReportOnly.
public export
lowConfidenceIsReport : (c : ConfidencePct) -> Not (LTE 85 c.value)
                     -> dispatch c = ReportOnly
lowConfidenceIsReport (MkPct v) not85 with (isLTE 95 v)
  lowConfidenceIsReport (MkPct v) not85 | Yes prf95 =
    absurd (not85 (lteTransitive (lteSuccRight (lteSuccRight (lteSuccRight
      (lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight
      (lteSuccRight (lteSuccRight lteRefl)))))))))) prf95))
  lowConfidenceIsReport (MkPct v) not85 | No _ with (isLTE 85 v)
    lowConfidenceIsReport (MkPct v) not85 | No _ | Yes prf85 = absurd (not85 prf85)
    lowConfidenceIsReport (MkPct v) not85 | No _ | No _ = Refl

------------------------------------------------------------------------
-- Section 5: Monotonicity proof
------------------------------------------------------------------------

||| Helper: autonomyLevel is monotone w.r.t. dispatch.
||| If confidence a >= confidence b, then autonomy(dispatch a) >= autonomy(dispatch b).
|||
||| We prove this via case analysis on threshold membership.

||| Helper: classify a confidence value into one of three zones.
public export
data Zone = ZoneHigh | ZoneMid | ZoneLow

public export
classify : ConfidencePct -> Zone
classify (MkPct v) =
  case isLTE 95 v of
    Yes _ => ZoneHigh
    No _  => case isLTE 85 v of
      Yes _ => ZoneMid
      No _  => ZoneLow

||| Helper: dispatch is consistent with classification.
public export
dispatchFromZone : (c : ConfidencePct) -> classify c = ZoneHigh -> dispatch c = AutoExecute
dispatchFromZone (MkPct v) prf with (isLTE 95 v)
  dispatchFromZone (MkPct v) prf | Yes _ = Refl
  dispatchFromZone (MkPct v) prf | No _ = absurd prf

public export
dispatchFromZoneMid : (c : ConfidencePct) -> classify c = ZoneMid -> dispatch c = Review
dispatchFromZoneMid (MkPct v) prf with (isLTE 95 v)
  dispatchFromZoneMid (MkPct v) prf | Yes _ = absurd prf
  dispatchFromZoneMid (MkPct v) prf | No _ with (isLTE 85 v)
    dispatchFromZoneMid (MkPct v) prf | No _ | Yes _ = Refl
    dispatchFromZoneMid (MkPct v) prf | No _ | No _ = absurd prf

public export
dispatchFromZoneLow : (c : ConfidencePct) -> classify c = ZoneLow -> dispatch c = ReportOnly
dispatchFromZoneLow (MkPct v) prf with (isLTE 95 v)
  dispatchFromZoneLow (MkPct v) prf | Yes _ = absurd prf
  dispatchFromZoneLow (MkPct v) prf | No _ with (isLTE 85 v)
    dispatchFromZoneLow (MkPct v) prf | No _ | Yes _ = absurd prf
    dispatchFromZoneLow (MkPct v) prf | No _ | No _ = Refl

||| Core monotonicity theorem:
||| For any two confidence values a and b, if a.value >= b.value,
||| then autonomyLevel(dispatch a) >= autonomyLevel(dispatch b).
|||
||| This is the key safety property: increasing confidence never
||| decreases the level of autonomy granted to the system.
public export
dispatchMonotone : (a : ConfidencePct) -> (b : ConfidencePct)
                -> LTE b.value a.value
                -> LTE (autonomyLevel (dispatch b)) (autonomyLevel (dispatch a))
dispatchMonotone (MkPct va) (MkPct vb) bLeqA with (isLTE 95 va)
  -- Case 1: a is in high zone (AutoExecute, autonomy = 2)
  dispatchMonotone (MkPct va) (MkPct vb) bLeqA | Yes _ with (isLTE 95 vb)
    -- b is also high
    dispatchMonotone (MkPct va) (MkPct vb) bLeqA | Yes _ | Yes _ = lteRefl
    -- b is not high
    dispatchMonotone (MkPct va) (MkPct vb) bLeqA | Yes _ | No _ with (isLTE 85 vb)
      -- b is mid (Review, autonomy = 1)
      dispatchMonotone (MkPct va) (MkPct vb) bLeqA | Yes _ | No _ | Yes _ =
        LTESucc LTEZero  -- 1 <= 2
      -- b is low (ReportOnly, autonomy = 0)
      dispatchMonotone (MkPct va) (MkPct vb) bLeqA | Yes _ | No _ | No _ =
        LTEZero  -- 0 <= 2
  -- Case 2: a is not in high zone
  dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA with (isLTE 85 va)
    -- a is in mid zone (Review, autonomy = 1)
    dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | Yes midA with (isLTE 95 vb)
      -- b is high: impossible because b.value <= a.value < 95
      dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | Yes midA | Yes highB =
        absurd (notHighA (lteTransitive highB bLeqA))
      -- b is not high
      dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | Yes midA | No _ with (isLTE 85 vb)
        -- b is also mid
        dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | Yes midA | No _ | Yes _ =
          lteRefl  -- 1 <= 1
        -- b is low
        dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | Yes midA | No _ | No _ =
          LTEZero  -- 0 <= 1
    -- a is in low zone (ReportOnly, autonomy = 0)
    dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | No notMidA with (isLTE 95 vb)
      -- b is high: impossible because b.value <= a.value < 85
      dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | No notMidA | Yes highB =
        absurd (notMidA (lteTransitive (lteSuccRight (lteSuccRight (lteSuccRight
          (lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight
          (lteSuccRight (lteSuccRight lteRefl)))))))))) (lteTransitive highB bLeqA)))
      -- b is not high
      dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | No notMidA | No _ with (isLTE 85 vb)
        -- b is mid: impossible because b.value <= a.value < 85
        dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | No notMidA | No _ | Yes midB =
          absurd (notMidA (lteTransitive midB bLeqA))
        -- b is also low: both ReportOnly
        dispatchMonotone (MkPct va) (MkPct vb) bLeqA | No notHighA | No notMidA | No _ | No _ =
          lteRefl  -- 0 <= 0

------------------------------------------------------------------------
-- Section 6: Annealing stage caps
------------------------------------------------------------------------

||| Annealing stages limit the maximum strategy a recipe can use.
public export
data Stage = Nascent | Adolescent | Mature | Veteran

||| Maximum strategy allowed per stage.
public export
maxStrategy : Stage -> Strategy
maxStrategy Nascent    = ReportOnly
maxStrategy Adolescent = Review
maxStrategy Mature     = AutoExecute
maxStrategy Veteran    = AutoExecute

||| Clamp a strategy to the stage maximum.
public export
clampToStage : Strategy -> Stage -> Strategy
clampToStage s stage =
  case isLTE (autonomyLevel (maxStrategy stage)) (autonomyLevel s) of
    Yes _ => case isLTE (autonomyLevel s) (autonomyLevel (maxStrategy stage)) of
      Yes _ => s         -- equal, keep it
      No _  => maxStrategy stage  -- s exceeds max, clamp down
    No _ => s            -- s is below max, keep it

||| Proof: clamping never exceeds the stage maximum.
public export
clampNeverExceedsMax : (s : Strategy) -> (stage : Stage)
                    -> LTE (autonomyLevel (clampToStage s stage))
                           (autonomyLevel (maxStrategy stage))
clampNeverExceedsMax ReportOnly  Nascent    = LTEZero
clampNeverExceedsMax Review      Nascent    = LTEZero
clampNeverExceedsMax AutoExecute Nascent    = LTEZero
clampNeverExceedsMax ReportOnly  Adolescent = LTEZero
clampNeverExceedsMax Review      Adolescent = lteRefl
clampNeverExceedsMax AutoExecute Adolescent = lteRefl
clampNeverExceedsMax ReportOnly  Mature     = LTEZero
clampNeverExceedsMax Review      Mature     = LTESucc LTEZero
clampNeverExceedsMax AutoExecute Mature     = lteRefl
clampNeverExceedsMax ReportOnly  Veteran    = LTEZero
clampNeverExceedsMax Review      Veteran    = LTESucc LTEZero
clampNeverExceedsMax AutoExecute Veteran    = lteRefl

||| Proof: nascent recipes can never auto-execute.
public export
nascentNeverAutoExecutes : (s : Strategy)
                        -> Not (clampToStage s Nascent = AutoExecute)
nascentNeverAutoExecutes ReportOnly  prf = absurd prf
nascentNeverAutoExecutes Review      prf = absurd prf
nascentNeverAutoExecutes AutoExecute prf = absurd prf

||| Proof: veteran recipes have no restrictions.
public export
veteranUnrestricted : (s : Strategy) -> clampToStage s Veteran = s
veteranUnrestricted ReportOnly  = Refl
veteranUnrestricted Review      = Refl
veteranUnrestricted AutoExecute = Refl
