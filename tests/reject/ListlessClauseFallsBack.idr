-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (RSR capability-profile resolution).
-- scripts/check-verdicts.sh passes only if Idris2 refuses this module, and
-- only at the declaration `invalid`.  Every other declaration must
-- typecheck, so that the refusal is for the stated reason and not for a
-- missing import or a typo.
--
-- Must fail: a single (rsr-profile …) clause without a :capabilities list
-- declares nothing.  It does not fall back to the rsr-profile.a2ml beside it.
--
-- EXPECT: Mismatch between: CapsList ["bash"] and CapsNone.

module ListlessClauseFallsBack

import Hypatia.ABI.RsrProfile

%default total
%unbound_implicits off

listlessDeed : List TopItem
listlessDeed = [ProfileClause [BodyField KOther ScalarValue]]

source : ProfileSource
source = MkProfileSource (Just ListlessClauseFallsBack.listlessDeed) (A2mlDeclares ["bash"])

invalid : declaredCapabilities ListlessClauseFallsBack.source = CapsList ["bash"]
invalid = Refl
