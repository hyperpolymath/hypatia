// SPDX-License-Identifier: PMPL-1.0-or-later
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Fuzz template validation
    if let Ok(s) = std::str::from_utf8(data) {
        // Add actual hypatia template validation fuzzing here
        // Example: hypatia_data::validate_template(s);
        let _ = s.len();
    }
});
