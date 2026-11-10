-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (RSR capability-profile resolution).
-- scripts/check-verdicts.sh passes only if Idris2 refuses this module, and
-- only at the declaration `invalid`.  Every other declaration must
-- typecheck, so that the refusal is for the stated reason and not for a
-- missing import or a typo.
--
-- Must fail: when the deed carries exactly one well-formed (rsr-profile …)
-- clause, its list is the profile; the legacy .a2ml beside it is not read.
--
-- EXPECT: Mismatch between: "container" and "bash".

module A2mlOverridesDeed

import Hypatia.ABI.RsrProfile

%default total
%unbound_implicits off

bashDeed : List TopItem
bashDeed = [ProfileClause [BodyField KCapabilities (ListValue [StrAtom "bash"])]]

source : ProfileSource
source = MkProfileSource (Just A2mlOverridesDeed.bashDeed) (A2mlDeclares ["container"])

invalid : declaredCapabilities A2mlOverridesDeed.source = CapsList ["container"]
invalid = Refl
