-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (RSR capability-profile resolution).
-- scripts/check-verdicts.sh passes only if Idris2 refuses this module, and
-- only at the declaration `invalid`.  Every other declaration must
-- typecheck, so that the refusal is for the stated reason and not for a
-- missing import or a typo.
--
-- Must fail: criterion 3.2.2 with no deed and an rsr-profile.a2ml that
-- exists but yields no capabilities list.  Before the 3.2.2 fix the
-- detector passed on the file's presence while the scorecard reported no
-- profile; 3.2.2 is now the verdict of the capability reader, so it fails.
--
-- EXPECT: Mismatch between: Pass and Fail.

module EmptyA2mlPassesProfileCriterion

import Hypatia.ABI.RsrProfile

%default total
%unbound_implicits off

source : ProfileSource
source = MkProfileSource Nothing A2mlDeclaresNothing

invalid : criterion322 EmptyA2mlPassesProfileCriterion.source = Pass
invalid = Refl
