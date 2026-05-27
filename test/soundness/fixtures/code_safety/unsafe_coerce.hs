-- SPDX-License-Identifier: MPL-2.0
-- SOUNDNESS FIXTURE — known-bad sample for code_safety/unsafe_coerce.
-- DO NOT FIX.

module Soundness.UnsafeCoerce where

import Unsafe.Coerce

-- TRUSTED: this `unsafeCoerce` is a deliberate scanner fixture covered
-- by test/soundness_test.exs (rule `code_safety/unsafe_coerce` must
-- fire on this file). Refutation budget = the soundness test suite.
bad :: Int -> String
bad n = unsafeCoerce n
