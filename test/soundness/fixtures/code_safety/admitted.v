(* SPDX-License-Identifier: MPL-2.0 *)
(* SOUNDNESS FIXTURE — known-bad sample for code_safety/admitted. *)
(* DO NOT FIX. *)

Theorem bad : 1 + 1 = 3.
Proof.
  Admitted.
