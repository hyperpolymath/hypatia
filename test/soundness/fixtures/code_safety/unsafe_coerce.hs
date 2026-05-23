-- SPDX-License-Identifier: MPL-2.0
-- SOUNDNESS FIXTURE — known-bad sample for code_safety/unsafe_coerce.
-- DO NOT FIX.

module Soundness.UnsafeCoerce where

import Unsafe.Coerce

bad :: Int -> String
bad n = unsafeCoerce n
