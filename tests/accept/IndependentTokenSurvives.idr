-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (HYP-01).  The counterpart of
-- tests/reject/RetractedSupportSurvives.idr: a finding corroborated by two
-- rules survives the quarantine of one of them, through the token of the
-- other; the token that rested on the quarantined rule does not.
-- test/retraction_test.exs runs the same case through the runtime mirror
-- (Hypatia.Verdict.Retraction).

module IndependentTokenSurvives

import Data.List.Elem
import Hypatia.ABI.Retraction

%default total
%unbound_implicits off

ruleA : Assumption
ruleA = RuleA "code_safety/rule_a"

ruleB : Assumption
ruleB = RuleA "code_safety/rule_b"

ctx : List Assumption
ctx = [ruleA, ruleB]

-- Quarantining rule A retracts the first assumption.
quarantineA : Retraction IndependentTokenSurvives.ctx [ruleB]
quarantineA = First

tokenA : Dependent IndependentTokenSurvives.ctx
tokenA = RestsOn [ruleA] (Keep (Drop Done))

tokenB : Dependent IndependentTokenSurvives.ctx
tokenB = RestsOn [ruleB] (Drop (Keep Done))

-- The finding survives, through token B.
findingSurvives : AnySurvives [ruleB] [tokenA, tokenB]
findingSurvives = ThereS (HereS (Keep Done))

-- What it survives by is token B's support, now under the retracted context.
survivesByB : support (survivor IndependentTokenSurvives.quarantineA IndependentTokenSurvives.findingSurvives) = [ruleB]
survivesByB = Refl

-- Token A does not survive: rule A is gone from [ruleB].
tokenADies : Not (Thin [ruleA] [ruleB])
tokenADies = retractedSupportDies Here (\e => case e of
  Here impossible
  There e' impossible)
