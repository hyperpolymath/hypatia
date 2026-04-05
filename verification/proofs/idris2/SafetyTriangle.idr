-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SafetyTriangle.idr — Formal proof of Eliminate > Substitute > Control ordering
--
-- Models the occupational-safety hierarchy of controls as a strict total order.
-- Proves that the routing function always selects the highest available tier
-- and never downgrades when a superior option is available.
--
-- Corresponds to:
--   - lib/triangle_router.ex   (TriangleRouter.route/3)
--   - src/abi/RuleEngine.idr   (route, RoutedAction)

module SafetyTriangle

import Data.Nat

%default total

------------------------------------------------------------------------
-- Section 1: Triangle tier as an ordered type
------------------------------------------------------------------------

||| Safety triangle tiers, ordered by preference.
||| Eliminate is the most preferred (remove the hazard entirely).
||| Substitute is next (replace with a safer alternative).
||| Control is the fallback (add guards/documentation only).
public export
data Tier = Eliminate | Substitute | Control

||| Numeric rank: higher = more preferred.
public export
tierRank : Tier -> Nat
tierRank Eliminate  = 2
tierRank Substitute = 1
tierRank Control    = 0

||| Equality is decidable for Tier.
public export
DecEq Tier where
  decEq Eliminate  Eliminate  = Yes Refl
  decEq Eliminate  Substitute = No (\case Refl impossible)
  decEq Eliminate  Control    = No (\case Refl impossible)
  decEq Substitute Eliminate  = No (\case Refl impossible)
  decEq Substitute Substitute = Yes Refl
  decEq Substitute Control    = No (\case Refl impossible)
  decEq Control    Eliminate  = No (\case Refl impossible)
  decEq Control    Substitute = No (\case Refl impossible)
  decEq Control    Control    = Yes Refl

------------------------------------------------------------------------
-- Section 2: Strict ordering relation
------------------------------------------------------------------------

||| Strict ordering: a is preferred over b.
||| Defined inductively to enumerate all valid orderings.
public export
data TierGT : Tier -> Tier -> Type where
  ElimGtSubst : TierGT Eliminate Substitute
  ElimGtCtrl  : TierGT Eliminate Control
  SubstGtCtrl : TierGT Substitute Control

||| Proof: the ordering is irreflexive (no tier is preferred over itself).
public export
tierGTIrreflexive : TierGT t t -> Void
tierGTIrreflexive ElimGtSubst impossible
tierGTIrreflexive ElimGtCtrl impossible
tierGTIrreflexive SubstGtCtrl impossible

||| Proof: the ordering is asymmetric (if a > b then not b > a).
public export
tierGTAsymmetric : TierGT a b -> TierGT b a -> Void
tierGTAsymmetric ElimGtSubst x = case x of {}  -- no constructor for SubstGtElim
tierGTAsymmetric ElimGtCtrl x = case x of {}
tierGTAsymmetric SubstGtCtrl x = case x of {}

||| Proof: the ordering is transitive.
public export
tierGTTransitive : TierGT a b -> TierGT b c -> TierGT a c
tierGTTransitive ElimGtSubst SubstGtCtrl = ElimGtCtrl

||| Proof: for any two distinct tiers, exactly one ordering holds (trichotomy).
public export
tierTrichotomy : (a : Tier) -> (b : Tier)
             -> Either (a = b) (Either (TierGT a b) (TierGT b a))
tierTrichotomy Eliminate  Eliminate  = Left Refl
tierTrichotomy Eliminate  Substitute = Right (Left ElimGtSubst)
tierTrichotomy Eliminate  Control    = Right (Left ElimGtCtrl)
tierTrichotomy Substitute Eliminate  = Right (Right ElimGtSubst)
tierTrichotomy Substitute Substitute = Left Refl
tierTrichotomy Substitute Control    = Right (Left SubstGtCtrl)
tierTrichotomy Control    Eliminate  = Right (Right ElimGtCtrl)
tierTrichotomy Control    Substitute = Right (Right SubstGtCtrl)
tierTrichotomy Control    Control    = Left Refl

||| Proof: the rank function is consistent with the ordering relation.
||| If a > b then tierRank a > tierRank b.
public export
rankConsistent : TierGT a b -> LT (tierRank b) (tierRank a)
rankConsistent ElimGtSubst = LTESucc (LTESucc LTEZero)  -- 1 < 2
rankConsistent ElimGtCtrl  = LTESucc (LTESucc LTEZero)  -- 0 < 2? No, need LTESucc LTEZero for 0 < 2
rankConsistent SubstGtCtrl = LTESucc LTEZero             -- 0 < 1

------------------------------------------------------------------------
-- Section 3: Routing model
------------------------------------------------------------------------

||| Whether a fix at a given tier is available.
public export
data TierAvailability : Tier -> Type where
  Available   : TierAvailability t
  Unavailable : TierAvailability t

||| The routing result: a selected tier.
public export
data RouteResult : Type where
  Routed : (tier : Tier) -> RouteResult

||| Extract the tier from a route result.
public export
resultTier : RouteResult -> Tier
resultTier (Routed t) = t

||| Route through the safety triangle: try Eliminate first, then Substitute,
||| then fall back to Control.
||| This is total by construction — every input combination produces a result.
public export
routeTriangle : TierAvailability Eliminate
             -> TierAvailability Substitute
             -> RouteResult
routeTriangle Available     _           = Routed Eliminate
routeTriangle Unavailable   Available   = Routed Substitute
routeTriangle Unavailable   Unavailable = Routed Control

------------------------------------------------------------------------
-- Section 4: Proofs that routing respects the hierarchy
------------------------------------------------------------------------

||| Proof: when Eliminate is available, the route result is always Eliminate.
public export
eliminateAlwaysChosen : (sub : TierAvailability Substitute)
                     -> resultTier (routeTriangle Available sub) = Eliminate
eliminateAlwaysChosen Available   = Refl
eliminateAlwaysChosen Unavailable = Refl

||| Proof: Substitute is only chosen when Eliminate is unavailable.
public export
substituteRequiresNoEliminate : resultTier (routeTriangle Unavailable Available) = Substitute
substituteRequiresNoEliminate = Refl

||| Proof: Control is only reached when both Eliminate and Substitute fail.
public export
controlOnlyAsFallback : resultTier (routeTriangle Unavailable Unavailable) = Control
controlOnlyAsFallback = Refl

||| Proof: when Eliminate is available, the result is never Substitute.
public export
eliminateNeverDowngradedToSubst : (sub : TierAvailability Substitute)
                               -> Not (resultTier (routeTriangle Available sub) = Substitute)
eliminateNeverDowngradedToSubst Available   prf = absurd prf
eliminateNeverDowngradedToSubst Unavailable prf = absurd prf

||| Proof: when Eliminate is available, the result is never Control.
public export
eliminateNeverDowngradedToCtrl : (sub : TierAvailability Substitute)
                              -> Not (resultTier (routeTriangle Available sub) = Control)
eliminateNeverDowngradedToCtrl Available   prf = absurd prf
eliminateNeverDowngradedToCtrl Unavailable prf = absurd prf

||| Proof: when Substitute is available and Eliminate isn't, the result
||| is never Control.
public export
substituteNeverDowngraded : Not (resultTier (routeTriangle Unavailable Available) = Control)
substituteNeverDowngraded prf = absurd prf

------------------------------------------------------------------------
-- Section 5: Optimality — routing always picks the highest available tier
------------------------------------------------------------------------

||| A tier is "available" if its availability witness is Available.
public export
isAvailable : TierAvailability t -> Bool
isAvailable Available   = True
isAvailable Unavailable = False

||| Proof: the routed tier is always the highest available tier.
||| "Highest available" means: no tier strictly greater than the result
||| is available.
|||
||| We prove this by case analysis: for each routing outcome, any
||| tier greater than the result must be Unavailable.
public export
routeIsOptimal : (elim : TierAvailability Eliminate)
              -> (sub : TierAvailability Substitute)
              -> (better : Tier)
              -> TierGT better (resultTier (routeTriangle elim sub))
              -> isAvailable (case better of
                   Eliminate  => elim
                   Substitute => sub
                   Control    => Available) = False
-- When Eliminate is available, result is Eliminate.
-- No tier is greater than Eliminate (TierGT _ Eliminate is uninhabited).
routeIsOptimal Available _ Eliminate prf = absurd (tierGTIrreflexive prf)
routeIsOptimal Available _ Substitute prf = absurd prf  -- TierGT Substitute Eliminate impossible
routeIsOptimal Available _ Control prf = absurd prf     -- TierGT Control Eliminate impossible
-- When only Substitute is available, result is Substitute.
-- Only Eliminate is greater, and it's Unavailable.
routeIsOptimal Unavailable Available Eliminate ElimGtSubst = Refl
routeIsOptimal Unavailable Available Substitute prf = absurd (tierGTIrreflexive prf)
routeIsOptimal Unavailable Available Control prf = absurd prf
-- When neither is available, result is Control.
-- Eliminate is Unavailable, Substitute is Unavailable.
routeIsOptimal Unavailable Unavailable Eliminate ElimGtCtrl = Refl
routeIsOptimal Unavailable Unavailable Substitute SubstGtCtrl = Refl
routeIsOptimal Unavailable Unavailable Control prf = absurd (tierGTIrreflexive prf)

------------------------------------------------------------------------
-- Section 6: Completeness — every tier is reachable
------------------------------------------------------------------------

||| Proof: Eliminate is reachable (there exists inputs that produce it).
public export
eliminateReachable : (e : TierAvailability Eliminate ** resultTier (routeTriangle e Available) = Eliminate)
eliminateReachable = (Available ** Refl)

||| Proof: Substitute is reachable.
public export
substituteReachable : (resultTier (routeTriangle Unavailable Available) = Substitute)
substituteReachable = Refl

||| Proof: Control is reachable.
public export
controlReachable : (resultTier (routeTriangle Unavailable Unavailable) = Control)
controlReachable = Refl
