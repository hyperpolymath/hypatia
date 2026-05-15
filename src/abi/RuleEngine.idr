-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia ABI — Rule Engine Proofs
--
-- Formal verification of the Hypatia rule engine invariants:
--   1. Rule totality:  every finding maps to exactly one action
--   2. Confidence monotonicity:  more evidence never decreases confidence
--   3. Safety triangle soundness:  eliminate > substitute > control hierarchy
--   4. Dispatch correctness:  findings route to the correct bot
--
-- These proofs correspond to the Elixir modules:
--   - lib/triangle_router.ex       (TriangleRouter)
--   - lib/fleet_dispatcher.ex      (FleetDispatcher)
--   - lib/confidence_annealing.ex  (ConfidenceAnnealing)
--   - lib/outcome_tracker.ex       (OutcomeTracker)
--
-- No proof-bypass primitives used.  %default total throughout.
-- hypatia:ignore code_safety/believe_me structural_drift/SD008
-- (the existing finding was the scanner counting the literal token
--  in this very comment line — there is no actual `believe_me` call.)

module Hypatia.ABI.RuleEngine

import Hypatia.ABI.Types
import Data.So
import Data.Nat

%default total

------------------------------------------------------------------------
-- Section 1: Safety Triangle — Types and Ordering
------------------------------------------------------------------------

||| The safety triangle has a strict total order:
|||   Eliminate > Substitute > Control
||| This mirrors the occupational-safety hierarchy of controls.
public export
data TriangleLT : TriangleTier -> TriangleTier -> Type where
  ||| Eliminate is strictly preferred over Substitute
  ElimGtSubst : TriangleLT Eliminate Substitute
  ||| Eliminate is strictly preferred over Control
  ElimGtCtrl  : TriangleLT Eliminate Control
  ||| Substitute is strictly preferred over Control
  SubstGtCtrl : TriangleLT Substitute Control

||| The triangle ordering is irreflexive: no tier is preferred over itself.
public export
triangleIrreflexive : TriangleLT t t -> Void
triangleIrreflexive ElimGtSubst impossible
triangleIrreflexive ElimGtCtrl impossible
triangleIrreflexive SubstGtCtrl impossible

||| The triangle ordering is transitive.
public export
triangleTransitive : TriangleLT a b -> TriangleLT b c -> TriangleLT a c
triangleTransitive ElimGtSubst SubstGtCtrl = ElimGtCtrl

||| Decision procedure: for any two distinct tiers, one is preferred.
public export
triangleCompare : (a : TriangleTier) -> (b : TriangleTier)
               -> Either (a = b) (Either (TriangleLT a b) (TriangleLT b a))
triangleCompare Eliminate  Eliminate  = Left Refl
triangleCompare Eliminate  Substitute = Right (Left ElimGtSubst)
triangleCompare Eliminate  Control    = Right (Left ElimGtCtrl)
triangleCompare Substitute Eliminate  = Right (Right ElimGtSubst)
triangleCompare Substitute Substitute = Left Refl
triangleCompare Substitute Control    = Right (Left SubstGtCtrl)
triangleCompare Control    Eliminate  = Right (Right ElimGtCtrl)
triangleCompare Control    Substitute = Right (Right SubstGtCtrl)
triangleCompare Control    Control    = Left Refl

------------------------------------------------------------------------
-- Section 2: Rule Totality — Every Finding Gets Exactly One Action
------------------------------------------------------------------------

||| A routed action is tagged with exactly one triangle tier.
||| This mirrors the Elixir return of {:eliminate, recipe, pattern},
||| {:substitute, recipe, pattern}, or {:control, pattern}.
public export
data RoutedAction : Type where
  ||| Hazard can be removed entirely (recipe found, auto_fixable or proven)
  RouteEliminate  : (recipe : Recipe) -> (pattern : Pattern) -> RoutedAction
  ||| Replace with a proven-safe module
  RouteSubstitute : (recipe : Recipe) -> (pattern : Pattern) -> RoutedAction
  ||| Add guards/documentation only (no safe fix available)
  RouteControl    : (pattern : Pattern) -> RoutedAction

||| Extract the triangle tier from a routed action.
public export
actionTier : RoutedAction -> TriangleTier
actionTier (RouteEliminate _ _)  = Eliminate
actionTier (RouteSubstitute _ _) = Substitute
actionTier (RouteControl _)      = Control

||| Whether an eliminate recipe was found for a pattern.
public export
data EliminateResult : Type where
  EliminateFound    : (recipe : Recipe) -> EliminateResult
  EliminateNotFound : EliminateResult

||| Whether a substitute recipe was found for a pattern.
public export
data SubstituteResult : Type where
  SubstituteFound    : (recipe : Recipe) -> SubstituteResult
  ||| Substitute search found an eliminate-tier recipe (promotion case)
  SubstitutePromoted : (recipe : Recipe) -> SubstituteResult
  SubstituteNotFound : SubstituteResult

||| Route a pattern through the safety triangle.
||| This function is total: every combination of eliminate/substitute
||| lookup results produces exactly one RoutedAction.
|||
||| Mirrors TriangleRouter.route/3 in lib/triangle_router.ex:
|||   1. Try eliminate first
|||   2. If not, try substitute (which may promote to eliminate)
|||   3. Fall back to control
public export
route : (pattern : Pattern) -> EliminateResult -> SubstituteResult -> RoutedAction
route pat (EliminateFound recipe) _                          = RouteEliminate recipe pat
route pat EliminateNotFound       (SubstituteFound recipe)   = RouteSubstitute recipe pat
route pat EliminateNotFound       (SubstitutePromoted recipe) = RouteEliminate recipe pat
route pat EliminateNotFound       SubstituteNotFound          = RouteControl pat

||| Proof: routing always respects the triangle hierarchy.
||| If eliminate is found, the action is Eliminate tier.
||| If only substitute is found, the action is Substitute tier.
||| If neither is found, the action is Control tier.
public export
routeRespectsHierarchy : (pat : Pattern)
                      -> (elim : EliminateResult)
                      -> (sub : SubstituteResult)
                      -> (actionTier (route pat elim sub) = Eliminate)
                         `Either`
                         ((actionTier (route pat elim sub) = Substitute)
                          `Either`
                          (actionTier (route pat elim sub) = Control))
routeRespectsHierarchy pat (EliminateFound recipe) _
  = Left Refl
routeRespectsHierarchy pat EliminateNotFound (SubstituteFound recipe)
  = Right (Left Refl)
routeRespectsHierarchy pat EliminateNotFound (SubstitutePromoted recipe)
  = Left Refl
routeRespectsHierarchy pat EliminateNotFound SubstituteNotFound
  = Right (Right Refl)

||| Proof: routing is total — every input combination produces a result.
||| This is witnessed by the function `route` itself being total (%default total),
||| but we additionally prove that the result is always one of the three tiers.
public export
routeTotality : (pat : Pattern)
             -> (elim : EliminateResult)
             -> (sub : SubstituteResult)
             -> (tier : TriangleTier ** actionTier (route pat elim sub) = tier)
routeTotality pat (EliminateFound recipe) _
  = (Eliminate ** Refl)
routeTotality pat EliminateNotFound (SubstituteFound recipe)
  = (Substitute ** Refl)
routeTotality pat EliminateNotFound (SubstitutePromoted recipe)
  = (Eliminate ** Refl)
routeTotality pat EliminateNotFound SubstituteNotFound
  = (Control ** Refl)

||| Proof: eliminate is always tried before substitute.
||| When eliminate succeeds, substitute result is irrelevant.
public export
eliminateFirst : (pat : Pattern) -> (recipe : Recipe)
              -> (sub : SubstituteResult)
              -> actionTier (route pat (EliminateFound recipe) sub) = Eliminate
eliminateFirst pat recipe _ = Refl

||| Proof: control is only reached when both eliminate and substitute fail.
public export
controlOnlyAsFallback : (pat : Pattern)
                     -> actionTier (route pat EliminateNotFound SubstituteNotFound) = Control
controlOnlyAsFallback pat = Refl

------------------------------------------------------------------------
-- Section 3: Confidence and Dispatch Strategy
------------------------------------------------------------------------

||| Confidence thresholds matching lib/triangle_router.ex:
|||   >= 0.95 -> AutoExecute
|||   >= 0.85 -> Review
|||   <  0.85 -> ReportOnly
public export
autoExecuteThreshold : Double
autoExecuteThreshold = 0.95

public export
reviewThreshold : Double
reviewThreshold = 0.85

||| Dispatch strategy determination.
||| Mirrors TriangleRouter.dispatch_strategy/1.
||| Total by construction: the three conditions are exhaustive over Double.
public export
dispatchStrategy : (confidence : Double) -> DispatchStrategy
dispatchStrategy confidence =
  if confidence >= 0.95 then AutoExecute
  else if confidence >= 0.85 then Review
  else ReportOnly

||| Proof: high confidence (>= 0.95) always yields AutoExecute.
public export
highConfidenceAutoExecutes : (c : Double)
                          -> So (c >= 0.95)
                          -> dispatchStrategy c = AutoExecute
highConfidenceAutoExecutes c prf with (c >= 0.95)
  highConfidenceAutoExecutes c Oh | True = Refl

||| Proof: medium confidence (>= 0.85, < 0.95) always yields Review.
public export
mediumConfidenceReviews : (c : Double)
                       -> So (not (c >= 0.95))
                       -> So (c >= 0.85)
                       -> dispatchStrategy c = Review
mediumConfidenceReviews c notHigh isMed with (c >= 0.95)
  mediumConfidenceReviews c notHigh isMed | False with (c >= 0.85)
    mediumConfidenceReviews c notHigh Oh | False | True = Refl

||| Proof: low confidence (< 0.85) always yields ReportOnly.
public export
lowConfidenceReports : (c : Double)
                    -> So (not (c >= 0.95))
                    -> So (not (c >= 0.85))
                    -> dispatchStrategy c = ReportOnly
lowConfidenceReports c notHigh notMed with (c >= 0.95)
  lowConfidenceReports c notHigh notMed | False with (c >= 0.85)
    lowConfidenceReports c notHigh Oh | False | False impossible
    lowConfidenceReports c notHigh notMed | False | False = Refl

------------------------------------------------------------------------
-- Section 4: Dispatch Correctness — Findings Route to the Right Bot
------------------------------------------------------------------------

||| The bot assigned for each dispatch strategy at the eliminate tier.
||| Mirrors FleetDispatcher.dispatch_eliminate_via_fleet/2.
public export
eliminateBot : DispatchStrategy -> BotId
eliminateBot AutoExecute = RobotRepoAutomaton
eliminateBot Review      = Rhodibot
eliminateBot ReportOnly  = Sustainabot

||| The bot(s) assigned for a substitute action.
||| Substitute always goes to Rhodibot (PR) + Echidnabot (proof obligation).
||| Mirrors FleetDispatcher.dispatch_routed_action({:substitute, ...}).
public export
data SubstituteDispatch : Type where
  MkSubstituteDispatch : (prBot : BotId)
                       -> (proofBot : BotId)
                       -> SubstituteDispatch

public export
substituteDispatch : SubstituteDispatch
substituteDispatch = MkSubstituteDispatch Rhodibot Echidnabot

||| The bot assigned for a control action.
||| Control always goes to Sustainabot (advisory).
||| Mirrors FleetDispatcher.dispatch_routed_action({:control, ...}).
public export
controlBot : BotId
controlBot = Sustainabot

||| Full dispatch: given a routed action, determine which bot(s) handle it.
||| This models the complete FleetDispatcher.dispatch_routed_action/1 function.
public export
data DispatchTarget : Type where
  ||| Single bot handles the action (eliminate or control)
  SingleBot  : BotId -> DispatchTarget
  ||| Two bots handle the action in parallel (substitute)
  DualBot    : BotId -> BotId -> DispatchTarget

public export
dispatch : RoutedAction -> DispatchTarget
dispatch (RouteEliminate recipe _) =
  let confidence = recipe.confidence
      strategy   = dispatchStrategy confidence
  in SingleBot (eliminateBot strategy)
dispatch (RouteSubstitute _ _) =
  DualBot Rhodibot Echidnabot
dispatch (RouteControl _) =
  SingleBot Sustainabot

||| Proof: auto-execute eliminate always dispatches to RobotRepoAutomaton.
public export
autoExecuteGoesToAutomaton : (recipe : Recipe) -> (pat : Pattern)
                          -> So (recipe.confidence >= 0.95)
                          -> dispatch (RouteEliminate recipe pat)
                             = SingleBot RobotRepoAutomaton
autoExecuteGoesToAutomaton recipe pat prf with (recipe.confidence >= 0.95)
  autoExecuteGoesToAutomaton recipe pat Oh | True = Refl

||| Proof: control always dispatches to Sustainabot.
public export
controlGoesToSustabot : (pat : Pattern)
                     -> dispatch (RouteControl pat) = SingleBot Sustainabot
controlGoesToSustabot pat = Refl

||| Proof: substitute always involves Rhodibot and Echidnabot.
public export
substituteInvolvesBothBots : (recipe : Recipe) -> (pat : Pattern)
                          -> dispatch (RouteSubstitute recipe pat)
                             = DualBot Rhodibot Echidnabot
substituteInvolvesBothBots recipe pat = Refl

------------------------------------------------------------------------
-- Section 5: Confidence Monotonicity (Bayesian Update)
------------------------------------------------------------------------

||| Bayesian Beta-distribution confidence model.
||| Mirrors OutcomeTracker.bayesian_update/3.
|||
||| posterior = (alpha_prior + successes) / (alpha_prior + successes + beta_prior + failures)
|||
||| where alpha_prior = prior * strength, beta_prior = (1 - prior) * strength.

||| Naturals-based model of the Bayesian update to avoid floating-point
||| reasoning.  We represent confidence as a rational alpha / (alpha + beta).
||| Prior strength is an implicit parameter (encoded in the initial alpha, beta).
public export
record BayesState where
  constructor MkBayes
  alpha : Nat  -- prior successes (scaled)
  beta  : Nat  -- prior failures (scaled)

||| Record a success: increment alpha.
public export
recordSuccess : BayesState -> BayesState
recordSuccess st = { alpha $= S } st

||| Record a failure: increment beta.
public export
recordFailure : BayesState -> BayesState
recordFailure st = { beta $= S } st

||| The "confidence numerator" is alpha.
||| The "confidence denominator" is alpha + beta.
||| Confidence = alpha / (alpha + beta).

||| Proof: recording a success never decreases the confidence numerator
||| while the denominator grows by the same amount — so the fraction
||| alpha / (alpha + beta) is non-decreasing.
|||
||| More precisely:  a / (a + b) <= (a + 1) / (a + 1 + b)
||| Cross-multiplying:  a * (a + 1 + b) <= (a + 1) * (a + b)
|||                     a^2 + a + ab    <= a^2 + ab + a + b
|||                     0               <= b
||| which holds for all natural b.
|||
||| We prove the cross-multiplication inequality directly on Nat.
public export
successNonDecreasing : (st : BayesState)
                    -> LTE (st.alpha * (S st.alpha + st.beta))
                           (S st.alpha * (st.alpha + st.beta))
successNonDecreasing (MkBayes a b) = successLemma a b
  where
    ||| Core lemma: a * (S a + b) <= S a * (a + b)
    ||| Expanding: a * S(a + b) <= S a * (a + b)
    ||| i.e.      a * (a + b) + a <= (a + b) + a * (a + b)
    ||| i.e.      a <= (a + b)
    ||| which is just LTE a (a + b), i.e. b >= 0.
    successLemma : (a : Nat) -> (b : Nat)
                -> LTE (a * (S a + b)) (S a * (a + b))
    successLemma Z b = LTEZero
    successLemma (S a') b = rewrite sym (plusSuccRightSucc a' b) in
      let -- S a' * (S (a' + b)) = S a' * S (a' + b)
          --                      = S (a' + b) + a' * S (a' + b)
          -- S (S a') * (S a' + b) = (S a' + b) + S a' * (S a' + b)
          -- We need: S a' * S(S a' + b) <= S(S a') * (S a' + b)
          -- This is: (S a') * (S (S a' + b)) <= (S (S a')) * (S a' + b)
          -- By induction on structure, this holds because
          -- the difference is exactly b >= 0.
          ih = successLemma a' b
      in lteAddRight ih

||| Proof: recording a failure never increases the confidence.
||| After failure, alpha stays the same but beta increases, so
||| alpha / (alpha + beta) >= alpha / (alpha + S beta).
||| Cross-multiplying: alpha * (alpha + S beta) >= alpha * (alpha + beta)
||| which simplifies to: alpha * (alpha + beta) + alpha >= alpha * (alpha + beta)
||| i.e. alpha >= 0, which holds for all Nat.
public export
failureNonIncreasing : (st : BayesState)
                    -> LTE (st.alpha * (st.alpha + S st.beta))
                           (st.alpha * (st.alpha + st.beta) + st.alpha)
failureNonIncreasing (MkBayes a b) = failureLemma a b
  where
    ||| a * (a + S b) = a * S(a + b) = a * (a + b) + a
    ||| So a * (a + S b) <= a * (a + b) + a is actually equality (LTE from reflexivity).
    failureLemma : (a : Nat) -> (b : Nat)
                -> LTE (a * (a + S b)) (a * (a + b) + a)
    failureLemma Z b = LTEZero
    failureLemma (S a') b =
      -- S a' * (S a' + S b) = S a' * S(a' + S b)
      -- Rewrite a + S b = S(a + b) so the multiplication aligns
      rewrite plusSuccRightSucc a' b in
        lteRefl

------------------------------------------------------------------------
-- Section 6: Safety Triangle Soundness
------------------------------------------------------------------------

||| The safety triangle is sound if the routing function always
||| assigns the highest-available tier.  That is:
|||
||| - If eliminate is available, the action tier is Eliminate.
||| - If only substitute is available, the action tier is Substitute.
||| - Otherwise, the action tier is Control.
|||
||| We also prove that no lower tier is assigned when a higher one
||| is available (no "downgrading").

||| Proof: when eliminate is available, route never produces Substitute or Control.
public export
eliminateNeverDowngraded : (pat : Pattern) -> (recipe : Recipe)
                        -> (sub : SubstituteResult)
                        -> Not (actionTier (route pat (EliminateFound recipe) sub) = Substitute)
eliminateNeverDowngraded pat recipe sub prf = absurd prf

||| Proof: when eliminate is available, route never produces Control.
public export
eliminateNeverControl : (pat : Pattern) -> (recipe : Recipe)
                     -> (sub : SubstituteResult)
                     -> Not (actionTier (route pat (EliminateFound recipe) sub) = Control)
eliminateNeverControl pat recipe sub prf = absurd prf

||| Proof: substitute is only chosen when eliminate is unavailable.
public export
substituteOnlyWithoutEliminate : (pat : Pattern)
                              -> (recipe : Recipe)
                              -> actionTier (route pat EliminateNotFound (SubstituteFound recipe))
                                 = Substitute
substituteOnlyWithoutEliminate pat recipe = Refl

||| Proof: a promoted substitute (eliminate-tier recipe found during
||| substitute search) correctly returns Eliminate tier, not Substitute.
public export
promotedSubstituteIsEliminate : (pat : Pattern)
                             -> (recipe : Recipe)
                             -> actionTier (route pat EliminateNotFound (SubstitutePromoted recipe))
                                = Eliminate
promotedSubstituteIsEliminate pat recipe = Refl

------------------------------------------------------------------------
-- Section 7: Dispatch Strategy Ordering
------------------------------------------------------------------------

||| Dispatch strategies have a trust ordering:
|||   AutoExecute > Review > ReportOnly
||| Higher trust = more autonomous action.
public export
data StrategyLT : DispatchStrategy -> DispatchStrategy -> Type where
  AutoGtReview  : StrategyLT AutoExecute Review
  AutoGtReport  : StrategyLT AutoExecute ReportOnly
  ReviewGtReport : StrategyLT Review ReportOnly

||| Strategy ordering is irreflexive.
public export
strategyIrreflexive : StrategyLT s s -> Void
strategyIrreflexive AutoGtReview impossible
strategyIrreflexive AutoGtReport impossible
strategyIrreflexive ReviewGtReport impossible

||| Strategy ordering is transitive.
public export
strategyTransitive : StrategyLT a b -> StrategyLT b c -> StrategyLT a c
strategyTransitive AutoGtReview ReviewGtReport = AutoGtReport

||| Annealing stage caps.
||| Mirrors ConfidenceAnnealing.max_dispatch_tier/1:
|||   nascent    -> ReportOnly
|||   adolescent -> Review
|||   mature     -> AutoExecute
|||   veteran    -> AutoExecute
public export
data AnnealingStage = Nascent | Adolescent | Mature | Veteran

public export
maxTier : AnnealingStage -> DispatchStrategy
maxTier Nascent    = ReportOnly
maxTier Adolescent = Review
maxTier Mature     = AutoExecute
maxTier Veteran    = AutoExecute

||| Strategy rank for comparison (0 = least trust, 2 = most).
public export
strategyRank : DispatchStrategy -> Nat
strategyRank ReportOnly  = 0
strategyRank Review      = 1
strategyRank AutoExecute = 2

||| Clamp a strategy to the maximum allowed by the annealing stage.
||| Mirrors ConfidenceAnnealing.clamp_strategy/2.
public export
clampStrategy : DispatchStrategy -> AnnealingStage -> DispatchStrategy
clampStrategy strategy stage =
  let maxAllowed = maxTier stage
  in if strategyRank strategy > strategyRank maxAllowed
     then maxAllowed
     else strategy

||| Proof: clamping never promotes a strategy above the stage maximum.
public export
clampNeverExceedsMax : (strategy : DispatchStrategy) -> (stage : AnnealingStage)
                    -> LTE (strategyRank (clampStrategy strategy stage))
                           (strategyRank (maxTier stage))
clampNeverExceedsMax ReportOnly  Nascent    = LTEZero
clampNeverExceedsMax Review      Nascent    = LTEZero
clampNeverExceedsMax AutoExecute Nascent    = LTEZero
clampNeverExceedsMax ReportOnly  Adolescent = LTEZero
clampNeverExceedsMax Review      Adolescent = lteRefl
clampNeverExceedsMax AutoExecute Adolescent = lteRefl
clampNeverExceedsMax ReportOnly  Mature     = LTEZero
clampNeverExceedsMax Review      Mature     = lteRefl
clampNeverExceedsMax AutoExecute Mature     = lteRefl
clampNeverExceedsMax ReportOnly  Veteran    = LTEZero
clampNeverExceedsMax Review      Veteran    = lteRefl
clampNeverExceedsMax AutoExecute Veteran    = lteRefl

||| Proof: nascent recipes can never auto-execute.
public export
nascentNeverAutoExecutes : (strategy : DispatchStrategy)
                        -> Not (clampStrategy strategy Nascent = AutoExecute)
nascentNeverAutoExecutes ReportOnly  prf = absurd prf
nascentNeverAutoExecutes Review      prf = absurd prf
nascentNeverAutoExecutes AutoExecute prf = absurd prf

||| Proof: veteran recipes have no dispatch restrictions.
public export
veteranUnrestricted : (strategy : DispatchStrategy)
                   -> clampStrategy strategy Veteran = strategy
veteranUnrestricted ReportOnly  = Refl
veteranUnrestricted Review      = Refl
veteranUnrestricted AutoExecute = Refl

------------------------------------------------------------------------
-- Section 8: End-to-End Composition
------------------------------------------------------------------------

||| The full pipeline: route a finding, then dispatch the routed action.
||| This composes Sections 2 and 4, proving the complete path from
||| finding to bot assignment is total and deterministic.
public export
pipeline : (pat : Pattern)
        -> (elim : EliminateResult)
        -> (sub : SubstituteResult)
        -> DispatchTarget
pipeline pat elim sub = dispatch (route pat elim sub)

||| Proof: the full pipeline always produces a dispatch target.
||| (Witnessed by `pipeline` being total under %default total.)
public export
pipelineTotality : (pat : Pattern)
                -> (elim : EliminateResult)
                -> (sub : SubstituteResult)
                -> (target : DispatchTarget ** pipeline pat elim sub = target)
pipelineTotality pat elim sub = (pipeline pat elim sub ** Refl)

||| Proof: control findings in the full pipeline always go to Sustainabot.
public export
controlPipelineTarget : (pat : Pattern)
                     -> pipeline pat EliminateNotFound SubstituteNotFound
                        = SingleBot Sustainabot
controlPipelineTarget pat = Refl

||| Proof: substitute findings in the full pipeline always involve both
||| Rhodibot (PR creation) and Echidnabot (proof obligation).
public export
substitutePipelineTarget : (pat : Pattern) -> (recipe : Recipe)
                        -> pipeline pat EliminateNotFound (SubstituteFound recipe)
                           = DualBot Rhodibot Echidnabot
substitutePipelineTarget pat recipe = Refl
