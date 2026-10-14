-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (HYP-01).  A `SoundVerdict` for the code_safety Lean
-- proof-hole detector, built from a Lean 4 re-check of that detector's
-- soundness fixture.  scripts/check-verdicts.sh requires Idris2 to ACCEPT
-- this module; the expected rejections in tests/reject/ are the other half.
--
-- The digests below are not trusted by being written here.
-- scripts/recheck-lean-hole.sh reads `fixturePath`, `proverVersion`,
-- `targetHash`, `artefactHash` and the span line out of this file, re-runs
-- Lean on the fixture, recomputes both digests, requires Lean's own
-- "declaration uses" diagnostic at the span line, and fails if any of them
-- disagrees with this file.  This module proves only that the record is
-- well-typed and is reported at level "sound"; the script ties it to a run.
--
-- Measured 2026-10-14 with Lean (version 4.30.0, x86_64-unknown-linux-gnu,
-- commit d024af099ca4bf2c86f649261ebf59565dc8c622, Release), run from the
-- repository root as `lean <fixturePath>`; the artefact is Lean's combined
-- stdout and stderr, exactly as printed.

module LeanHoleRecheckSound

import Hypatia.ABI.Verdict

%default total
%unbound_implicits off

-- The fixture the detector must flag and Lean must re-check.
fixturePath : String
fixturePath = "test/soundness/fixtures/code_safety/sorry.lean" -- hypatia: allow code_safety/sorry -- fixture path as data, not a proof hole

-- The detector's rule id (lib/rules/code_safety.ex, @lean_banned).
detectorRuleId : String
detectorRuleId = "sorry" -- hypatia: allow code_safety/sorry -- rule id as data, not a proof hole

||| The Lean 4 re-check of the fixture.
public export
recheck : ProverRecheck
recheck = MkProverRecheck
  Lean4
  "4.30.0"
  fixturePath
  (MkSha256 "5465a2710136d470469e3bc4351c27dd0a6d8a14f520fd7009f5f196351c72e7") -- targetHash
  (MkSha256 "f5d4cf9e8b96dd98cbf47d432fb83411dad3aa92dd37de1f21d30bebf1f03a31") -- artefactHash
  Confirms

||| The detector's evidence: rule, version, the flagged line, one observation
||| (this fixture).  `ruleVersion` is the hypatia version in mix.exs.
public export
holeToken : EvidenceToken
holeToken = MkEvidenceToken "code_safety" detectorRuleId "0.1.0"
          (MkSpan fixturePath 8 8) 1

||| The claim the verdict makes.
public export
holeClaim : String
holeClaim = fixturePath ++ ":8: theorem `bad` rests on a Lean proof hole"

||| The control: the detector's token, supported by the Lean re-check.
public export
soundControl : SoundVerdict
soundControl = MkSoundVerdict (MkVerdict holeClaim [holeToken] (Mechanical recheck))
                         (Rechecked recheck Refl)

||| The control is reported at level "sound", by evaluation.
public export
controlIsSound : levelName (level (verdict soundControl)) = "sound"
controlIsSound = Refl

||| The control's soundness witness names the measured artefact.
public export
controlArtefact : hex (soundArtefact (sound soundControl))
                = "f5d4cf9e8b96dd98cbf47d432fb83411dad3aa92dd37de1f21d30bebf1f03a31"
controlArtefact = Refl

||| The same token held without the re-check is epi, not sound: the match
||| alone is evidence that the pattern matched.
public export
patternOnlyIsEpi : levelName (level (MkVerdict holeClaim [holeToken] Pattern)) = "epi"
patternOnlyIsEpi = Refl

||| The same claim with no evidence token is belief.
public export
noTokenIsBelief : levelName (level (MkVerdict holeClaim [] (Observed 5))) = "belief"
noTokenIsBelief = Refl
