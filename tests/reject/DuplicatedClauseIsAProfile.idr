-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (RSR capability-profile resolution).
-- scripts/check-verdicts.sh passes only if Idris2 refuses this module, and
-- only at the declaration `invalid`.  Every other declaration must
-- typecheck, so that the refusal is for the stated reason and not for a
-- missing import or a typo.
--
-- Must fail: a deed carrying two top-level (rsr-profile …) clauses declares
-- nothing, so the scorecard cannot report a profile as present, even with an
-- rsr-profile.a2ml beside it that declares one.
--
-- EXPECT: Mismatch between: True and False.

module DuplicatedClauseIsAProfile

import Hypatia.ABI.RsrProfile

%default total
%unbound_implicits off

dupDeed : List TopItem
dupDeed =
  [ ProfileClause [BodyField KCapabilities (ListValue [StrAtom "bash"])]
  , ProfileClause [BodyField KCapabilities (ListValue [StrAtom "rust"])]
  ]

source : ProfileSource
source = MkProfileSource (Just DuplicatedClauseIsAProfile.dupDeed) (A2mlDeclares ["bash"])

invalid : scorecardProfilePresent (declaredCapabilities DuplicatedClauseIsAProfile.source) = True
invalid = Refl
