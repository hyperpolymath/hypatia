// SPDX-License-Identifier: MPL-2.0
// SOUNDNESS FIXTURE — known-bad sample for code_safety/zig_ptr_cast.
// DO NOT FIX.

pub fn bad(ptr: *u8) *u32 {
    return @ptrCast(*u32, ptr);
}
