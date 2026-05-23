// SPDX-License-Identifier: MPL-2.0
// SOUNDNESS FIXTURE — known-bad sample for code_safety/getexn_on_external.
// DO NOT FIX.

let bad = (untrusted: Js.Dict.t<string>) => Js.Dict.getExn(untrusted, "key")
