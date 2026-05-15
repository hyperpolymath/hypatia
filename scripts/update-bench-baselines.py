#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0-or-later
# hypatia:ignore cicd_rules/banned_language_file
# Intentional exception to the org's no-Python policy: this script
# parses criterion's bencher-format output and rewrites the JSON
# baseline file. The criterion ecosystem (and most Rust bench tooling)
# emits Python-friendly text; rewriting in shell or Rust is possible
# but not yet a priority. Tracked in the .hypatia-exemptions.md table.
"""
update-bench-baselines.py — regenerate
.machine_readable/benchmarks/baselines.json from a criterion bencher-
format run.

Usage:
    cargo bench --bench hypatia_bench -- \\
        --warm-up-time 1 --measurement-time 2 --sample-size 10 \\
        --output-format bencher > /tmp/bench.txt
    python3 scripts/update-bench-baselines.py \\
        /tmp/bench.txt .machine_readable/benchmarks/baselines.json

Parses lines of the form:

    test <name> ... bench:       12,345 ns/iter (+/- 678)

and writes a JSON object keyed by bench name → median `ns/iter` into
the baselines.json at the path given as the second argument, preserving
the `_comment`, `_schema_version`, and `_regression_threshold_pct`
keys of the existing file. If the target file is missing those
metadata keys, sane defaults are written.
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
    """Extract {name: ns_per_iter} from criterion bencher-format output."""
    out: dict[str, int] = {}
    for line in text.splitlines():
        m = BENCH_LINE.match(line.strip())
        if m:
            ns = int(m.group("ns").replace(",", ""))
            out[m.group("name")] = ns
    return out


def load_existing(path: Path) -> dict:
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text())
    except json.JSONDecodeError:
        return {}


def write_baselines(target: Path, new_baselines: dict[str, int]) -> None:
    existing = load_existing(target)
    merged = {
        "_comment": existing.get(
            "_comment",
            "Per-benchmark baseline in ns/iter. Keys are criterion bench "
            "names; values are the median ns/iter recorded on a main-branch "
            "run.",
        ),
        "_schema_version": existing.get("_schema_version", 1),
        "_regression_threshold_pct": existing.get(
            "_regression_threshold_pct", 50
        ),
        "baselines": new_baselines,
    }
    target.write_text(json.dumps(merged, indent=2, sort_keys=False) + "\n")


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(
            "usage: update-bench-baselines.py <bencher-output> <baselines.json>",
            file=sys.stderr,
        )
        return 2

    source = Path(argv[1])
    target = Path(argv[2])

    if not source.exists():
        print(f"error: source {source} does not exist", file=sys.stderr)
        return 1

    new_baselines = parse_bencher_output(source.read_text())
    if not new_baselines:
        print(
            "error: no `test <name> ... bench: ...` lines matched — did "
            "criterion run with --output-format bencher?",
            file=sys.stderr,
        )
        return 1

    write_baselines(target, new_baselines)
    print(
        f"wrote {len(new_baselines)} baselines to {target}",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
