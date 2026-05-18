// SPDX-License-Identifier: PMPL-1.0-or-later
//
// update-bench-baselines — regenerate
// .machine_readable/benchmarks/baselines.json from a criterion bencher-
// format run, preserving the existing `_comment`, `_schema_version` and
// `_regression_threshold_pct` metadata. A faithful Rust port of the former
// scripts/update-bench-baselines.py (org policy bans Python outside
// SaltStack). Pairs with check-bench-regression.
//
// Usage:
//     update-bench-baselines <bencher-output> <baselines.json>
//
// Exit status:  0 = wrote baselines,  1 = source missing / no bench lines,
//               2 = usage error.

use bench_tools::{parse_bencher_output, parse_json, to_pretty, Json};
use std::process::exit;

const DEFAULT_COMMENT: &str = "Per-benchmark baseline in ns/iter. Keys are criterion bench \
names; values are the median ns/iter recorded on a main-branch run.";

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    if argv.len() != 3 {
        eprintln!("usage: update-bench-baselines <bencher-output> <baselines.json>");
        exit(2);
    }
    let source = &argv[1];
    let target = &argv[2];

    let source_text = match std::fs::read_to_string(source) {
        Ok(t) => t,
        Err(_) => {
            eprintln!("error: source {source} does not exist");
            exit(1);
        }
    };

    let new_baselines = parse_bencher_output(&source_text);
    if new_baselines.is_empty() {
        eprintln!(
            "error: no `test <name> ... bench: ...` lines matched \u{2014} \
             did criterion run with --output-format bencher?"
        );
        exit(1);
    }

    // Load existing metadata (missing / invalid -> empty, mirroring Python).
    let existing = std::fs::read_to_string(target)
        .ok()
        .and_then(|t| parse_json(&t).ok())
        .unwrap_or(Json::Obj(vec![]));
    let keep = |k: &str, default: Json| existing.get(k).cloned().unwrap_or(default);

    let merged = Json::Obj(vec![
        ("_comment".into(), keep("_comment", Json::Str(DEFAULT_COMMENT.into()))),
        ("_schema_version".into(), keep("_schema_version", Json::Num("1".into()))),
        (
            "_regression_threshold_pct".into(),
            keep("_regression_threshold_pct", Json::Num("50".into())),
        ),
        (
            "baselines".into(),
            Json::Obj(
                new_baselines
                    .iter()
                    .map(|(n, ns)| (n.clone(), Json::Num(ns.to_string())))
                    .collect(),
            ),
        ),
    ]);

    if let Err(e) = std::fs::write(target, to_pretty(&merged)) {
        eprintln!("error: could not write {target}: {e}");
        exit(1);
    }
    eprintln!("wrote {} baselines to {target}", new_baselines.len());
}
