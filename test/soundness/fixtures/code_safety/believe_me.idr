-- SPDX-License-Identifier: MPL-2.0
-- SOUNDNESS FIXTURE — known-bad sample for code_safety/believe_me.
-- DO NOT FIX. This file exists so the build fails if the rule stops firing.

module Soundness.BelieveMe

-- TRUSTED: this `believe_me` is a deliberate scanner fixture covered
-- by test/soundness_test.exs (rule `code_safety/believe_me` must fire
-- on this file). Refutation budget = the soundness test suite.
bad : Nat
bad = believe_me Z
