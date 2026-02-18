// SPDX-License-Identifier: PMPL-1.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Fuzz registry operations
    if let Ok(s) = std::str::from_utf8(data) {
        // Add actual hypatia registry operation fuzzing here
        let _ = s.parse::<String>();
    }
});
