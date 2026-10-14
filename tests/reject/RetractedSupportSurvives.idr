-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (HYP-01).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- Must fail: quarantining the rule retracts the only assumption the token
-- rests on, so its support [rule] does not thin into the empty context and
-- the token cannot survive (residual-evidence-types tests/reject/
-- FalseRetraction, transcribed).
--
-- EXPECT: Mismatch between: [?x] and [].

module RetractedSupportSurvives

import Hypatia.ABI.Retraction

%default total
%unbound_implicits off

rule : Assumption
rule = RuleA "code_safety/example_rule"

-- The token rests on the rule that fired.
token : Dependent [rule]
token = RestsOn [rule] (Keep Done)

invalid : Dependent []
invalid = survivesDependent (First {x = rule} {g = []}) token (Keep Done)
