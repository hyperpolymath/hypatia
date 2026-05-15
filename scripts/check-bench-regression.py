#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0-or-later
# hypatia:ignore cicd_rules/banned_language_file
# Intentional exception to the org's no-Python policy: this script
# parses criterion's bencher-format output and compares against the
# committed baseline. Pairs with update-bench-baselines.py.
# Tracked in the .hypatia-exemptions.md table.
"""
check-bench-regression.py — compare a criterion bencher run against
.machine_readable/benchmarks/baselines.json and fail if any benchmark
has regressed by more than the configured threshold.

Usage:
    python3 scripts/check-bench-regression.py \\
        /tmp/bench.txt \\
        .machine_readable/benchmarks/baselines.json

Exit status:
    0 — no regressions exceed the threshold (or baseline is empty).
    1 — at least one benchmark exceeds the threshold.
    2 — usage / file error.

Writes a Markdown-formatted summary to stdout suitable for
`$GITHUB_STEP_SUMMARY`. Emits `::error::` annotations for regressions
so GitHub annotates the offending job.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path


BENCH_LINE = re.compile(
    r"^test\s+(?P<name>\S+)\s+\.\.\.\s+bench:\s+(?P<ns>[\d,]+)\s+ns/iter"
)


def parse_bencher_output(text: str) -> dict[str, int]:
    out: dict[str, int] = {}
    for line in text.splitlines():
        m = BENCH_LINE.match(line.strip())
        if m:
            out[m.group("name")] = int(m.group("ns").replace(",", ""))
    return out


def fmt_ns(ns: int) -> str:
    if ns >= 1_000_000:
        return f"{ns / 1_000_000:.2f} ms"
    if ns >= 1_000:
        return f"{ns / 1_000:.2f} µs"
    return f"{ns} ns"


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(
            "usage: check-bench-regression.py <bencher-output> <baselines.json>",
            file=sys.stderr,
        )
        return 2

    current_path = Path(argv[1])
    baselines_path = Path(argv[2])

    if not current_path.exists():
        print(f"error: {current_path} missing", file=sys.stderr)
        return 2

    current = parse_bencher_output(current_path.read_text())

    if not current:
        print(
            "::warning::no bench lines parsed from current run — "
            "did criterion use --output-format bencher?"
        )
        return 0

    baseline_doc = {}
    if baselines_path.exists():
        try:
            baseline_doc = json.loads(baselines_path.read_text())
        except json.JSONDecodeError:
            print(
                f"::warning::{baselines_path} is not valid JSON; "
                "treating as empty baseline"
            )

    baselines = baseline_doc.get("baselines", {}) or {}
    threshold_pct = float(baseline_doc.get("_regression_threshold_pct", 50))

    if not baselines:
        print("## Benchmark run (advisory mode — no baselines yet)")
        print()
        print("| Benchmark | Current |")
        print("|-----------|---------|")
        for name, ns in sorted(current.items()):
            print(f"| `{name}` | {fmt_ns(ns)} |")
        print()
        print(
            "_No entries in `baselines.json` yet — see "
            "`.machine_readable/benchmarks/README.md` for how to seed them._"
        )
        return 0

    # Compare
    regressions: list[tuple[str, int, int, float]] = []
    report_rows: list[tuple[str, str, str, str, str]] = []

    for name, ns_now in sorted(current.items()):
        ns_base = baselines.get(name)
        if ns_base is None:
            report_rows.append(
                (name, fmt_ns(ns_now), "—", "new", "✨")
            )
            continue

        pct = (ns_now - ns_base) / ns_base * 100 if ns_base else 0.0
        verdict = "✅"
        if pct > threshold_pct:
            verdict = "❌"
            regressions.append((name, ns_base, ns_now, pct))
        elif pct > threshold_pct / 2:
            verdict = "⚠️"
        elif pct < -10:
            verdict = "🚀"

        report_rows.append(
            (name, fmt_ns(ns_now), fmt_ns(ns_base), f"{pct:+.1f}%", verdict)
        )

    print("## Benchmark comparison")
    print()
    print(f"Threshold: regression > **{threshold_pct:.0f}%** fails CI.")
    print()
    print("| Benchmark | Current | Baseline | Δ | |")
    print("|-----------|---------|----------|---|---|")
    for row in report_rows:
        print(f"| `{row[0]}` | {row[1]} | {row[2]} | {row[3]} | {row[4]} |")
    print()

    if regressions:
        print("### Regressions exceeding threshold")
        print()
        for name, ns_base, ns_now, pct in regressions:
            msg = (
                f"{name}: {fmt_ns(ns_base)} → {fmt_ns(ns_now)} "
                f"({pct:+.1f}%, threshold {threshold_pct:.0f}%)"
            )
            print(f"- {msg}")
            # GitHub annotation on stderr so the summary on stdout stays clean.
            print(f"::error::benchmark regression: {msg}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
