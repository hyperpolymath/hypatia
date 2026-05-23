// SPDX-License-Identifier: MPL-2.0
// SOUNDNESS FIXTURE — known-bad sample for code_safety/transmute.
// DO NOT FIX.

pub fn bad(x: u32) -> f32 {
    unsafe { std::mem::transmute(x) }
}
