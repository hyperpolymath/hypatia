// SPDX-License-Identifier: PMPL-1.0-or-later
//! Benchmarks for Hypatia's core fixer operations.
//!
//! Exercises real logic: error catalog construction and lookup, SHA pin resolution,
//! scan result aggregation, and regex-based workflow pattern scanning.

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use cicd_fixer::{ErrorCatalog, ShaPins, ScanResult, Issue, IssueCategory, IssueSeverity};

/// Benchmark constructing the full error catalog from the embedded pattern set.
fn bench_catalog_construction(c: &mut Criterion) {
    c.bench_function("error_catalog_construction", |b| {
        b.iter(|| {
            let catalog = ErrorCatalog::new();
            black_box(catalog.all().len())
        });
    });
}

/// Benchmark looking up individual error patterns by ID (HashMap lookup).
fn bench_catalog_lookup(c: &mut Criterion) {
    let catalog = ErrorCatalog::default();
    let ids = [
        "token-permissions-id",
        "pinned-dependencies-id",
        "missing-spdx-header",
        "hard-coded-cryptographic-value",
        "fuzzing-id",
        "nonexistent-pattern-id",
    ];

    c.bench_function("catalog_lookup_6_ids", |b| {
        b.iter(|| {
            let mut count = 0usize;
            for id in &ids {
                if catalog.get(black_box(id)).is_some() {
                    count += 1;
                }
            }
            black_box(count)
        });
    });
}

/// Benchmark filtering catalog patterns by severity.
fn bench_catalog_filter_by_severity(c: &mut Criterion) {
    let catalog = ErrorCatalog::default();

    c.bench_function("catalog_filter_critical", |b| {
        b.iter(|| {
            let critical = catalog.by_severity(black_box(cicd_fixer::catalog::Severity::Critical));
            black_box(critical.len())
        });
    });

    c.bench_function("catalog_auto_fixable_filter", |b| {
        b.iter(|| {
            let fixable = catalog.auto_fixable();
            black_box(fixable.len())
        });
    });
}

/// Benchmark constructing the SHA pin registry and performing lookups.
fn bench_sha_pin_lookup(c: &mut Criterion) {
    let sha_pins = ShaPins::default();
    let actions = [
        "actions/checkout",
        "github/codeql-action/init",
        "dtolnay/rust-toolchain",
        "ossf/scorecard-action",
        "Swatinem/rust-cache",
        "nonexistent/action",
    ];

    c.bench_function("sha_pin_lookup_6_actions", |b| {
        b.iter(|| {
            let mut found = 0usize;
            for action in &actions {
                if sha_pins.get_pin(black_box(action)).is_some() {
                    found += 1;
                }
            }
            black_box(found)
        });
    });
}

/// Benchmark SHA pin registry construction (building the full HashMap).
fn bench_sha_pin_construction(c: &mut Criterion) {
    c.bench_function("sha_pin_registry_construction", |b| {
        b.iter(|| {
            let pins = ShaPins::new();
            black_box(pins)
        });
    });
}

/// Benchmark scan result summary generation with realistic issue counts.
fn bench_scan_result_summary(c: &mut Criterion) {
    // Build a realistic scan result with mixed severity issues
    let issues: Vec<Issue> = (0..50)
        .map(|i| Issue {
            id: format!("issue-{}", i),
            severity: match i % 4 {
                0 => IssueSeverity::Critical,
                1 => IssueSeverity::High,
                2 => IssueSeverity::Medium,
                _ => IssueSeverity::Low,
            },
            file_path: format!(".github/workflows/workflow-{}.yml", i),
            line_number: Some(i + 1),
            description: format!("Test issue number {}", i),
            fix_suggestion: "Apply standard fix".to_string(),
            auto_fixable: i % 3 != 0,
            category: match i % 5 {
                0 => IssueCategory::UnpinnedAction,
                1 => IssueCategory::MissingPermissions,
                2 => IssueCategory::MissingSpdx,
                3 => IssueCategory::MissingToolchainInput,
                _ => IssueCategory::Other,
            },
        })
        .collect();
    let auto_fixable_count = issues.iter().filter(|i| i.auto_fixable).count();
    let scan_result = ScanResult {
        repo_path: "/tmp/test-repo".to_string(),
        workflows_scanned: 17,
        issues,
        auto_fixable_count,
    };

    c.bench_function("scan_result_summary_50_issues", |b| {
        b.iter(|| {
            let summary = scan_result.summary();
            black_box(summary)
        });
    });
}

/// Benchmark full fixer construction (catalog + SHA pins + scanner).
fn bench_fixer_construction(c: &mut Criterion) {
    c.bench_function("cicd_fixer_full_construction", |b| {
        b.iter(|| {
            let fixer = cicd_fixer::CicdFixer::new();
            black_box(fixer.catalog.all().len());
        });
    });
}

criterion_group!(
    benches,
    bench_catalog_construction,
    bench_catalog_lookup,
    bench_catalog_filter_by_severity,
    bench_sha_pin_lookup,
    bench_sha_pin_construction,
    bench_scan_result_summary,
    bench_fixer_construction,
);
criterion_main!(benches);
