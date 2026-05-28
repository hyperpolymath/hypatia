-- SPDX-License-Identifier: MPL-2.0
-- SOUNDNESS FIXTURE — known-bad sample for code_safety/agda_postulate.
-- DO NOT FIX.

-- TRUSTED: this `postulate` is a deliberate scanner fixture covered by
-- test/soundness_test.exs (rule `code_safety/agda_postulate` must fire
-- on this file). Refutation budget = the soundness test suite.
postulate
  bad : Set
