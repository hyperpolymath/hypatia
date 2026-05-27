(* SPDX-License-Identifier: MPL-2.0 *)
(* SOUNDNESS FIXTURE — known-bad sample for code_safety/admitted. *)
(* DO NOT FIX. *)

(* TRUSTED: this `Admitted.` is a deliberate scanner fixture covered  *)
(* by test/soundness_test.exs (rule `code_safety/admitted` must fire   *)
(* on this file). Refutation budget = the soundness test suite.       *)
Theorem bad : 1 + 1 = 3.
Proof.
  Admitted.
