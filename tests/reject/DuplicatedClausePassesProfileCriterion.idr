-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (RSR capability-profile resolution).
-- scripts/check-verdicts.sh passes only if Idris2 refuses this module, and
-- only at the declaration `invalid`.  Every other declaration must
-- typecheck, so that the refusal is for the stated reason and not for a
-- missing import or a typo.
--
-- Must fail: criterion 3.2.2 on a deed carrying two top-level
-- (rsr-profile …) clauses.  Before bffd76a the detector passed on any clause
-- while the capability reader declared nothing; the model fails it.
--
-- EXPECT: Mismatch between: Pass and Fail.

module DuplicatedClausePassesProfileCriterion

import Hypatia.ABI.RsrProfile

%default total
%unbound_implicits off

dupDeed : List TopItem
dupDeed =
  [ ProfileClause [BodyField KCapabilities (ListValue [StrAtom "bash"])]
  , ProfileClause [BodyField KCapabilities (ListValue [StrAtom "rust"])]
  ]

source : ProfileSource
source = MkProfileSource (Just DuplicatedClausePassesProfileCriterion.dupDeed) (A2mlDeclares ["bash"])

invalid : criterion322 DuplicatedClausePassesProfileCriterion.source = Pass
invalid = Refl
