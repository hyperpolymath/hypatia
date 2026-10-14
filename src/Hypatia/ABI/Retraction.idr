-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia ABI — Quarantine as retraction (HYP-01)
--
-- Quarantining a bot or a rule retracts an assumption.  Each evidence token
-- a finding was issued on rests on assumptions: the rule that fired and,
-- for a fleet finding, the bot that reported it; a confirming prover
-- re-check rests on the prover.  A token survives a retraction exactly when
-- its support thins into what remains, and a finding survives while one of
-- its tokens does.  lib/hypatia/verdict/retraction.ex is the runtime mirror.
--
-- What is proved here (Idris2, %default total, no proof-bypass primitives):
--   * `retractThin`: a retraction is a thinning read downwards;
--   * `survivesDependent`: a token whose support thins into the retracted
--     context still rests on the same support there;
--   * `thinElem` / `retractedSupportDies`: a token whose support names an
--     assumption absent from the remaining context does not survive;
--   * `survivor` / `noTokensNoSurvival`: a finding survives exactly through
--     one of its tokens, and a finding with no tokens has nothing to survive
--     by.
--
-- Transcription source (read for the shape only, never imported):
--   hyperpolymath/residual-evidence-types @ 97244f5cb63bd6697caff261b187b62e9788b692
--   src/ResidualEvidence/Context.agda  — the thinning _⊆_ (done / keep /
--     drop), ⊆-refl, ⊆-trans, no-thinning-into-empty
--   src/ResidualEvidence/Revision.agda — Retraction (first / later),
--     retract-⊆, Dependent, survives-dependent
-- Hypatia cannot state a finding's claim as a type, so `Dependent` keeps the
-- support and its thinning and drops the `Holds` field: what retraction acts
-- on is the naming of the support, and that is what is transcribed.  For the
-- same reason Revision.agda's dependent-holds (a Dependent gives back Holds)
-- has no counterpart here.  The
-- correspondence is ASSUMED, not checked; scripts/check-sibling-citations.sh
-- checks only that the cited names exist at the pin.

module Hypatia.ABI.Retraction

import Data.List.Elem

%default total

------------------------------------------------------------------------
-- Section 1: Assumptions and thinnings
------------------------------------------------------------------------

||| What an evidence token can rest on.
public export
data Assumption = RuleA String | BotA String | ProverA String

||| Order-preserving embedding: `Thin s g` says `g` assumes everything `s`
||| does, in order, and possibly more (Context.agda `_⊆_`).
public export
data Thin : List a -> List a -> Type where
  Done : Thin [] []
  Keep : Thin s g -> Thin (x :: s) (x :: g)
  Drop : Thin s g -> Thin s (x :: g)

public export
thinRefl : {g : List a} -> Thin g g
thinRefl {g = []} = Done
thinRefl {g = x :: g} = Keep thinRefl

public export
thinTrans : Thin s g -> Thin g h -> Thin s h
thinTrans t Done = t
thinTrans t (Drop t') = Drop (thinTrans t t')
thinTrans (Keep t) (Keep t') = Keep (thinTrans t t')
thinTrans (Drop t) (Keep t') = Drop (thinTrans t t')

||| A non-empty context never thins into the empty one.
public export
noThinIntoEmpty : Thin (x :: s) [] -> Void
noThinIntoEmpty Done impossible
noThinIntoEmpty (Keep _) impossible
noThinIntoEmpty (Drop _) impossible

||| Every assumption of the smaller context occurs in the larger one.
public export
thinElem : Elem x s -> Thin s g -> Elem x g
thinElem Here (Keep _) = Here
thinElem (There e) (Keep t) = There (thinElem e t)
thinElem e (Drop t) = There (thinElem e t)

------------------------------------------------------------------------
-- Section 2: Retraction
------------------------------------------------------------------------

||| `d` is `g` with exactly one assumption retracted, named by its position
||| (Revision.agda `Retraction`).
public export
data Retraction : List a -> List a -> Type where
  First : Retraction (x :: g) g
  Later : Retraction g d -> Retraction (x :: g) (x :: d)

||| The assumption a retraction removes.
public export
retracted : {0 a : Type} -> {g, d : List a} -> Retraction g d -> a
retracted (First {x}) = x
retracted (Later r) = retracted r

||| A retraction is a thinning of the smaller context into the larger one
||| (Revision.agda `retract-⊆`).
public export
retractThin : {d : List a} -> Retraction g d -> Thin d g
retractThin First = Drop thinRefl
retractThin {d = _ :: _} (Later r) = Keep (retractThin r)

------------------------------------------------------------------------
-- Section 3: Tokens rest on their support
------------------------------------------------------------------------

||| An evidence token under context `g`, naming the support it rests on
||| (Revision.agda `Dependent`, without `Holds`).
public export
record Dependent (g : List a) where
  constructor RestsOn
  support : List a
  thin    : Thin support g

||| The whole context is always a support (Context.agda `⊆-refl`).
public export
restsOnAll : (g : List a) -> Dependent g
restsOnAll g = RestsOn g thinRefl

||| A token whose support thins into the retracted context survives, resting
||| on the same support (Revision.agda `survives-dependent`).  The premise is
||| against the retracted context `d`, never against `g`.
public export
survivesDependent : Retraction g d -> (dep : Dependent g)
                 -> Thin (support dep) d -> Dependent d
survivesDependent _ (RestsOn s _) avoid = RestsOn s avoid

||| What retraction cannot do: a support naming an assumption that no longer
||| occurs in the remaining context does not thin into it.
public export
retractedSupportDies : Elem x s -> Not (Elem x d) -> Not (Thin s d)
retractedSupportDies e absent t = absent (thinElem e t)

------------------------------------------------------------------------
-- Section 4: Findings survive through a token
------------------------------------------------------------------------

||| Some token of a finding survives into `d`.
public export
data AnySurvives : (d : List a) -> List (Dependent g) -> Type where
  HereS  : Thin (support t) d -> AnySurvives d (t :: ts)
  ThereS : AnySurvives d ts -> AnySurvives d (t :: ts)

||| A finding with no tokens has nothing to survive by.
public export
noTokensNoSurvival : AnySurvives d [] -> Void
noTokensNoSurvival (HereS _) impossible
noTokensNoSurvival (ThereS _) impossible

||| A surviving finding hands back the token it survives by, as a token of
||| the retracted context.
public export
survivor : Retraction g d -> {ts : List (Dependent g)} -> AnySurvives d ts -> Dependent d
survivor r {ts = t :: _} (HereS avoid) = survivesDependent r t avoid
survivor r {ts = _ :: _} (ThereS rest) = survivor r rest
