// SPDX-License-Identifier: PMPL-1.0-or-later
//
// check-bench-regression — compare a criterion bencher run against
// .machine_readable/benchmarks/baselines.json and fail if any benchmark
// regressed by more than the configured threshold. A faithful Rust port of
// the former scripts/check-bench-regression.py (org policy bans Python
// outside SaltStack). Pairs with update-bench-baselines.
//
// Usage:
//     check-bench-regression <bencher-output> <baselines.json>
//
// Exit status:  0 = no regressions over threshold (or no baselines yet),
//               1 = at least one regression,  2 = usage / file error.
//
// Markdown summary -> stdout (for $GITHUB_STEP_SUMMARY); `::error::`
// annotations -> stderr.

use bench_tools::{fmt_ns, parse_bencher_output, parse_json, Json};
use std::process::exit;

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    if argv.len() != 3 {
        eprintln!("usage: check-bench-regression <bencher-output> <baselines.json>");
        exit(2);
    }
    let current_path = &argv[1];
    let baselines_path = &argv[2];

    let current_text = match std::fs::read_to_string(current_path) {
        Ok(t) => t,
        Err(_) => {
            eprintln!("error: {current_path} missing");
            exit(2);
        }
    };

    let mut current = parse_bencher_output(&current_text);
    current.sort_by(|a, b| a.0.cmp(&b.0)); // Python iterates `sorted(current.items())`

    if current.is_empty() {
        println!(
            "::warning::no bench lines parsed from current run \u{2014} \
             did criterion use --output-format bencher?"
        );
        exit(0);
    }

    let baseline_doc: Json = match std::fs::read_to_string(baselines_path) {
        Ok(t) => match parse_json(&t) {
            Ok(v) => v,
            Err(_) => {
                println!(
                    "::warning::{baselines_path} is not valid JSON; \
                     treating as empty baseline"
                );
                Json::Obj(vec![])
            }
        },
        Err(_) => Json::Obj(vec![]),
    };

    let baselines: Vec<(String, f64)> = match baseline_doc.get("baselines") {
        Some(Json::Obj(p)) => p
            .iter()
            .filter_map(|(k, v)| v.as_f64().map(|n| (k.clone(), n)))
            .collect(),
        _ => vec![],
    };
    let lookup = |name: &str| baselines.iter().find(|(k, _)| k == name).map(|(_, v)| *v);

    let threshold_pct = baseline_doc
        .get("_regression_threshold_pct")
        .and_then(|v| v.as_f64())
        .unwrap_or(50.0);

    if baselines.is_empty() {
        println!("## Benchmark run (advisory mode \u{2014} no baselines yet)");
        println!();
        println!("| Benchmark | Current |");
        println!("|-----------|---------|");
        for (name, ns) in &current {
            println!("| `{name}` | {} |", fmt_ns(*ns));
        }
        println!();
        println!(
            "_No entries in `baselines.json` yet \u{2014} see \
             `.machine_readable/benchmarks/README.md` for how to seed them._"
        );
        exit(0);
    }

    let mut regressions: Vec<(String, i64, i64, f64)> = vec![];
    let mut rows: Vec<(String, String, String, String, String)> = vec![];

    for (name, ns_now) in &current {
        let ns_now = *ns_now;
        match lookup(name) {
            None => rows.push((
                name.clone(),
                fmt_ns(ns_now),
                "\u{2014}".into(),
                "new".into(),
                "\u{2728}".into(),
            )),
            Some(ns_base) => {
                let pct = if ns_base != 0.0 {
                    (ns_now as f64 - ns_base) / ns_base * 100.0
                } else {
                    0.0
                };
                let mut verdict = "\u{2705}";
                if pct > threshold_pct {
                    verdict = "\u{274c}";
                    regressions.push((name.clone(), ns_base as i64, ns_now, pct));
                } else if pct > threshold_pct / 2.0 {
                    verdict = "\u{26a0}\u{fe0f}";
                } else if pct < -10.0 {
                    verdict = "\u{1f680}";
                }
                rows.push((
                    name.clone(),
                    fmt_ns(ns_now),
                    fmt_ns(ns_base as i64),
                    format!("{pct:+.1}%"),
                    verdict.into(),
                ));
            }
        }
    }

    println!("## Benchmark comparison");
    println!();
    println!("Threshold: regression > **{threshold_pct:.0}%** fails CI.");
    println!();
    println!("| Benchmark | Current | Baseline | \u{0394} | |");
    println!("|-----------|---------|----------|---|---|");
    for (a, b, c, d, e) in &rows {
        println!("| `{a}` | {b} | {c} | {d} | {e} |");
    }
    println!();

    if !regressions.is_empty() {
        println!("### Regressions exceeding threshold");
        println!();
        for (name, ns_base, ns_now, pct) in &regressions {
            let msg = format!(
                "{name}: {} \u{2192} {} ({pct:+.1}%, threshold {threshold_pct:.0}%)",
                fmt_ns(*ns_base),
                fmt_ns(*ns_now),
            );
            println!("- {msg}");
            eprintln!("::error::benchmark regression: {msg}");
        }
        exit(1);
    }
}
