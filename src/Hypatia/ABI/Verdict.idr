-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia ABI — Verdict records (HYP-01)
--
-- Every gate verdict names its support.  A verdict is an Epi: a claim, the
-- evidence tokens it was issued on (rule id and version, matched span, fleet
-- observation count), and the support those tokens give.  It is sound only
-- when the support carries a soundness map from evidence to the claim, and
-- the only such map hypatia has today is a mechanical falsifier: a prover
-- re-checking the file a proof-bypass detector flagged.  A pattern match is
-- evidence that the pattern matched; an observation count is belief
-- formation (rules.a2ml proposes a rule at 5 observations); neither is a
-- proof that the property holds.
--
-- What is proved here (Idris2, %default total, no proof-bypass primitives):
--   * `Sound` has exactly one constructor, indexed by `Mechanical`, and it
--     demands a re-check whose result `Confirms` the claim
--     (`patternNotSound`, `observedNotSound`, `refutedNotSound`);
--   * `decSound` decides soundness of any support;
--   * the derived `level` says "sound" only when a `Sound` witness exists
--     (`soundLevelHasWitness`, `patternNeverSoundLevel`,
--     `observedNeverSoundLevel`), and a `SoundVerdict` always derives "sound"
--     (`soundVerdictLevel`);
--   * a `Sha256` literal that is not 64 lower-case hex digits does not
--     typecheck, so a `Sound` witness always names a well-formed digest of
--     its re-check artefact (`soundArtefact`).
--
-- What is NOT proved here:
--   * that a `Confirms` result is what the prover actually printed: that is
--     the job of scripts/recheck-lean-hole.sh, which re-runs the prover and
--     compares digests with the positive control in tests/accept/;
--   * that this record corresponds to the Agda definitions it transcribes.
--
-- Transcription source (read for the shape only, never imported):
--   hyperpolymath/epistemic-types @ 6a68de3141a981d766fd006b312f0919ade334bf
--   src/EpistemicTypes/Warrant.agda — Warrant, Epi, SoundWarrant, sound-epi
--   src/EpistemicTypes/Applications/RapidNJSkip.agda — no-receipt-reflection
-- Correspondence (Warrant ~ the rule at a version; Epi ~ `Verdict`;
-- SoundWarrant ~ `SoundVerdict`; sound-epi ~ `Rechecked`) is ASSUMED, not
-- checked.  scripts/fetch-siblings.sh fetches the pin and
-- scripts/check-sibling-citations.sh fails if a name cited above is absent
-- there; neither checks the correspondence itself.

module Hypatia.ABI.Verdict

import public Data.So

%default total

------------------------------------------------------------------------
-- Section 1: Digests
------------------------------------------------------------------------

||| A lower-case hexadecimal digit.
public export
isLowerHexDigit : Char -> Bool
isLowerHexDigit c = isDigit c || (c >= 'a' && c <= 'f')

||| Exactly 64 lower-case hex digits: the shape of a SHA-256 digest.
public export
isSha256Hex : String -> Bool
isSha256Hex s = length s == 64 && all isLowerHexDigit (unpack s)

||| A SHA-256 digest.  The well-formedness proof is found by evaluation, so a
||| literal of the wrong shape is refused at compile time.
public export
record Sha256 where
  constructor MkSha256
  hex : String
  {auto 0 wellFormed : So (isSha256Hex hex)}

------------------------------------------------------------------------
-- Section 2: Evidence tokens
------------------------------------------------------------------------

||| A matched region of a file, 1-based and inclusive.
public export
record Span where
  constructor MkSpan
  file      : String
  startLine : Nat
  endLine   : Nat

||| One piece of evidence a rule offered for a verdict: which rule, at which
||| version, matched where, and how many times the fleet has observed it.
||| A token is evidence that the rule fired.  It is not the claim.
public export
record EvidenceToken where
  constructor MkEvidenceToken
  ruleModule   : String
  ruleId       : String
  ruleVersion  : String
  span         : Span
  observations : Nat

------------------------------------------------------------------------
-- Section 3: Prover re-checks
------------------------------------------------------------------------

||| The provers whose bypass detectors hypatia ships
||| (lib/rules/code_safety.ex; fixtures in test/soundness/fixtures/code_safety/).
public export
data Prover = Lean4 | Idris2 | Agda | Coq

||| What a prover reported when it re-checked a flagged file.
|||   Confirms — the prover's own diagnostic states the bypass the detector
|||              claimed (Lean 4: "declaration uses `...`" at the flagged line).
|||   Refutes  — the prover re-checked the file and reported no bypass: the
|||              detector's match was not a proof hole.
public export
data RecheckResult = Confirms | Refutes

public export
Uninhabited (Refutes = Confirms) where
  uninhabited Refl impossible

public export
Uninhabited (Confirms = Refutes) where
  uninhabited Refl impossible

||| Decide whether a result confirms.
public export
decConfirms : (r : RecheckResult) -> Dec (r = Confirms)
decConfirms Confirms = Yes Refl
decConfirms Refutes  = No absurd

||| A prover re-check of one file.  `targetHash` is the digest of the bytes
||| the prover read; `artefactHash` is the digest of everything the prover
||| printed.  Both are what scripts/recheck-lean-hole.sh recomputes.
public export
record ProverRecheck where
  constructor MkProverRecheck
  prover        : Prover
  proverVersion : String
  target        : String
  targetHash    : Sha256
  artefactHash  : Sha256
  result        : RecheckResult

------------------------------------------------------------------------
-- Section 4: Support
------------------------------------------------------------------------

||| What a verdict's evidence supports.
public export
data Support : Type where
  ||| A pattern matched.  Evidence that the pattern matched, and nothing more.
  Pattern    : Support
  ||| The finding has been observed `n` times across the fleet.  A count is
  ||| belief formation; rules.a2ml's proposal threshold of 5 is a policy, not
  ||| a soundness proof.
  Observed   : (n : Nat) -> Support
  ||| A prover re-checked the flagged file.
  Mechanical : (recheck : ProverRecheck) -> Support

||| `Sound s`: the support `s` carries a soundness map from its evidence to
||| the claim.  The only constructor is indexed by `Mechanical` and demands a
||| re-check that `Confirms`.  This is SoundWarrant's `sound` field read at
||| the ABI layer: the prover's own report is the map.
public export
data Sound : Support -> Type where
  Rechecked : (recheck : ProverRecheck)
           -> (confirmed : result recheck = Confirms)
           -> Sound (Mechanical recheck)

||| The digest of the re-check artefact a `Sound` witness rests on.
public export
soundArtefact : Sound s -> Sha256
soundArtefact (Rechecked recheck _) = artefactHash recheck

||| A `Sound` witness names exactly its re-check's artefact.
public export
soundArtefactIsRecheck : (w : Sound (Mechanical recheck))
                      -> soundArtefact w = artefactHash recheck
soundArtefactIsRecheck (Rechecked recheck _) = Refl

||| A pattern match is never sound.
public export
patternNotSound : Sound Pattern -> Void
patternNotSound (Rechecked _ _) impossible

||| No observation count is sound, however large.
public export
observedNotSound : Sound (Observed n) -> Void
observedNotSound (Rechecked _ _) impossible

||| A re-check that refutes the detector is not soundness either.
public export
refutedNotSound : (recheck : ProverRecheck)
               -> result recheck = Refutes
               -> Sound (Mechanical recheck) -> Void
refutedNotSound recheck refuted (Rechecked recheck confirmed) =
  absurd (trans (sym refuted) confirmed)

||| Soundness of a support is decidable.
public export
decSound : (s : Support) -> Dec (Sound s)
decSound Pattern = No patternNotSound
decSound (Observed n) = No observedNotSound
decSound (Mechanical recheck) with (decConfirms (result recheck))
  decSound (Mechanical recheck) | Yes confirmed = Yes (Rechecked recheck confirmed)
  decSound (Mechanical recheck) | No notConfirmed =
    No (\(Rechecked recheck confirmed) => notConfirmed confirmed)

------------------------------------------------------------------------
-- Section 5: Verdicts and their level
------------------------------------------------------------------------

||| A gate verdict: the claim, the evidence tokens it was issued on, and the
||| support they give.  Pass/fail is derived from this record, never stored
||| beside it.  This is Epi read at the ABI layer: holding a `Verdict` does
||| not by itself give its claim.
public export
record Verdict where
  constructor MkVerdict
  claim    : String
  evidence : List EvidenceToken
  support  : Support

||| A verdict with a soundness witness for its support: SoundWarrant read at
||| the ABI layer.
public export
record SoundVerdict where
  constructor MkSoundVerdict
  verdict : Verdict
  sound   : Sound (support verdict)

||| The epistemic level a verdict is reported at.  These are the only three
||| values the findings schema accepts for `level`.
|||   LvSound  — the support carries a soundness witness;
|||   LvEpi    — evidence tokens, no soundness witness;
|||   LvBelief — no evidence token at all (a Bayesian posterior alone).
public export
data Level = LvSound | LvEpi | LvBelief

public export
Uninhabited (LvEpi = LvSound) where
  uninhabited Refl impossible

public export
Uninhabited (LvBelief = LvSound) where
  uninhabited Refl impossible

||| Wire spelling of a level (schema/findings.ncl `Level`).
public export
levelName : Level -> String
levelName LvSound  = "sound"
levelName LvEpi    = "epi"
levelName LvBelief = "belief"

||| The level without a soundness witness: tokens make it epi, none make it
||| belief.
public export
unsoundLevel : List EvidenceToken -> Level
unsoundLevel []       = LvBelief
unsoundLevel (_ :: _) = LvEpi

||| An unsound level is never "sound".
public export
unsoundLevelNotSound : (ts : List EvidenceToken) -> unsoundLevel ts = LvSound -> Void
unsoundLevelNotSound []       prf = absurd prf
unsoundLevelNotSound (_ :: _) prf = absurd prf

||| Level from a soundness decision.
public export
levelFrom : Dec (Sound s) -> List EvidenceToken -> Level
levelFrom (Yes _) _  = LvSound
levelFrom (No _)  ts = unsoundLevel ts

||| The level a verdict is reported at, derived from its support.
public export
level : Verdict -> Level
level v = levelFrom (decSound (support v)) (evidence v)

||| "sound" from `levelFrom` means the decision found a witness.
public export
levelFromSoundHasWitness : (d : Dec (Sound s)) -> (ts : List EvidenceToken)
                        -> levelFrom d ts = LvSound -> Sound s
levelFromSoundHasWitness (Yes w) _  _   = w
levelFromSoundHasWitness (No _)  ts prf = absurd (unsoundLevelNotSound ts prf)

||| A verdict reported at level "sound" has a soundness witness.
public export
soundLevelHasWitness : (v : Verdict) -> level v = LvSound -> Sound (support v)
soundLevelHasWitness v = levelFromSoundHasWitness (decSound (support v)) (evidence v)

||| A pattern-supported verdict is never reported "sound".
public export
patternNeverSoundLevel : (v : Verdict) -> support v = Pattern
                      -> level v = LvSound -> Void
patternNeverSoundLevel v supp prf =
  patternNotSound (replace {p = Sound} supp (soundLevelHasWitness v prf))

||| An observation-supported verdict is never reported "sound".
public export
observedNeverSoundLevel : (v : Verdict) -> support v = Observed n
                       -> level v = LvSound -> Void
observedNeverSoundLevel v supp prf =
  observedNotSound (replace {p = Sound} supp (soundLevelHasWitness v prf))

||| A decision that has a witness in hand reports "sound".
public export
levelFromWitness : (w : Sound s) -> (d : Dec (Sound s)) -> (ts : List EvidenceToken)
                -> levelFrom d ts = LvSound
levelFromWitness _ (Yes _)      _ = Refl
levelFromWitness w (No contra)  _ = absurd (contra w)

||| A `SoundVerdict` is always reported "sound".
public export
soundVerdictLevel : (sv : SoundVerdict) -> level (verdict sv) = LvSound
soundVerdictLevel sv =
  levelFromWitness (sound sv) (decSound (support (verdict sv))) (evidence (verdict sv))

||| Forget the witness: every sound verdict is a verdict.
public export
forgetSound : SoundVerdict -> Verdict
forgetSound = verdict
