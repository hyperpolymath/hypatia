// SPDX-License-Identifier: MPL-2.0
//
// check-k9iser-paths — parse k9iser.toml and verify every declared
// `[[source]]` path exists on disk. A faithful Rust port of the inline
// `python3` step formerly embedded in ci.yml (org policy bans Python
// outside SaltStack; no exceptions).
//
// Usage:  check-k9iser-paths [k9iser.toml]   (defaults to ./k9iser.toml)
// Exit:   0 = all declared paths exist, 1 = one or more missing,
//         2 = usage / parse error.

use std::path::Path;
use std::process::exit;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    let manifest = argv.get(1).map(String::as_str).unwrap_or("k9iser.toml");

    let text = match std::fs::read_to_string(manifest) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: cannot read {manifest}: {e}");
            exit(2);
        }
    };
    let doc: toml::Value = match toml::from_str(&text) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error: {manifest} is not valid TOML: {e}");
            exit(2);
        }
    };

    let sources = doc
        .get("source")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    // Missing or non-array "constraint" means zero constraints declared —
    // that's a legitimate audit reading (zero is the absent state), not a
    // silent error swallow. Counter to the rule's "dangerous default" framing,
    // 0 is the only sensible default here.
    let constraints = doc
        .get("constraint")
        .and_then(|v| v.as_array())
        .map_or(0, |a| a.len());

    let mut missing: Vec<String> = Vec::new();
    for src in &sources {
        if let Some(p) = src.get("path").and_then(|v| v.as_str()) {
            if !Path::new(p).exists() {
                missing.push(p.to_string());
            }
        }
    }

    if !missing.is_empty() {
        for m in &missing {
            println!("::error::{manifest} declares missing source {m}");
        }
        exit(1);
    }

    println!(
        "OK  {manifest} parses — {} source(s), {} constraint(s)",
        sources.len(),
        constraints
    );
}
