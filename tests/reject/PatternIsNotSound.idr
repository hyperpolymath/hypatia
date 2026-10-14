-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (HYP-01).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- Must fail: a pattern match is evidence that the pattern matched, not a
-- soundness map: `Sound` is indexed only by `Mechanical`, so a `Sound` witness
-- for a `Pattern` support does not unify.
--
-- EXPECT: Mismatch between: Mechanical recheck and Pattern.

module PatternIsNotSound

import Hypatia.ABI.Verdict

%default total

-- Placeholder digests: well-formed, and deliberately not the digest of
-- anything.  They are scaffolding, not evidence.
zero : Sha256
zero = MkSha256 "0000000000000000000000000000000000000000000000000000000000000000"

recheck : ProverRecheck
recheck = MkProverRecheck Lean4 "4.30.0" "Example.lean" zero zero Confirms

refuted : ProverRecheck
refuted = MkProverRecheck Lean4 "4.30.0" "Example.lean" zero zero Refutes

token : EvidenceToken
token = MkEvidenceToken "code_safety" "example_rule" "0.1.0"
          (MkSpan "Example.lean" 1 1) 1

invalid : SoundVerdict
invalid = MkSoundVerdict (MkVerdict "claim" [token] Pattern) (Rechecked recheck Refl)
