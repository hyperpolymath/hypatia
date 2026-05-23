-- SPDX-License-Identifier: MPL-2.0
-- SOUNDNESS FIXTURE — known-bad sample for code_safety/believe_me.
-- DO NOT FIX. This file exists so the build fails if the rule stops firing.

module Soundness.BelieveMe

bad : Nat
bad = believe_me Z
