// SPDX-License-Identifier: MPL-2.0
// SOUNDNESS FIXTURE — known-bad sample for code_safety/unwrap_without_check.
// DO NOT FIX.

pub fn bad(s: &str) -> i32 {
    s.parse::<i32>().unwrap()
}
