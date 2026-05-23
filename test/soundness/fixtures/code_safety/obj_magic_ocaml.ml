(* SPDX-License-Identifier: MPL-2.0 *)
(* SOUNDNESS FIXTURE — known-bad sample for code_safety/obj_magic_ocaml. *)
(* DO NOT FIX. *)

let bad (x : int) : string = Obj.magic x
