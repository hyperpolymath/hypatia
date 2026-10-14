-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (HYP-01).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- Must fail: a `Sound` witness must name a well-formed SHA-256 digest of
-- its artefact.  `MkSha256` finds its well-formedness proof by evaluation, so a
-- digest that is not 64 lower-case hex digits does not typecheck.
--
-- EXPECT: Can't find an implementation for So False.

module MalformedDigest

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

invalid : Sha256
invalid = MkSha256 "F5D4CF9E"
