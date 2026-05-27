-- SPDX-License-Identifier: MPL-2.0
-- SOUNDNESS FIXTURE — known-bad sample for code_safety/sorry.
-- DO NOT FIX.

-- TRUSTED: this `sorry` is a deliberate scanner fixture covered by
-- test/soundness_test.exs (rule `code_safety/sorry` must fire on this
-- file). Refutation budget = the soundness test suite.
theorem bad : 1 + 1 = 3 := by sorry
