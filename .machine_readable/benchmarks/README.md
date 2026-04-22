# Benchmark baselines

This directory holds the per-benchmark baselines that `.github/workflows/bench.yml`
compares against. The file that does the gating is
`baselines.json` — a keyed map of criterion bench names to their
median `ns/iter` on a known-good main-branch run.

## Why this exists

Before these files, criterion benches (in `fixer/benches/hypatia_bench.rs`)
ran locally only and were never compared across commits. That is a
"scaffolded" state in the Hyperpolymath Six Sigma benchmark taxonomy —
the infrastructure exists but no discipline around it.

This directory upgrades the state to "partial":
* Baselines are versioned data.
* PR runs compare against the committed baseline.
* Regressions above the threshold fail CI.
* Regenerating baselines is a deliberate action, not drift.

## Files

| File | Purpose |
|------|---------|
| `baselines.json` | Current baselines. CI reads this on every PR. |
| `README.md` (this file) | Convention documentation. |

## Schema

```json
{
  "_schema_version": 1,
  "_regression_threshold_pct": 50,
  "baselines": {
    "error_catalog_construction": 12345,
    "catalog_lookup_6_ids": 678,
    "catalog_filter_critical": 910
  }
}
```

- `_schema_version` — bump on breaking shape changes.
- `_regression_threshold_pct` — percent above baseline that triggers
  a CI failure. Default 50% per TESTING-TAXONOMY.adoc's Six Sigma
  "Acceptable / Unacceptable" boundary.
- `baselines` — map of criterion bench name → median `ns/iter`.
  Missing entries are treated as "new benchmark, no baseline yet,
  pass".

## How the gate works

`bench.yml` runs on every PR touching `fixer/**` or the workflow itself:

1. Runs `cargo bench --bench hypatia_bench -- --output-format bencher`
   with quick-mode timing (warm-up 1s, measurement 2s, sample 10)
   so the whole suite completes in ~2 minutes.
2. Parses the output for `test <name> ... bench: <ns> ns/iter ...`.
3. For each bench, compares to `baselines.json`:
   * If baseline absent — log current, pass (new benchmark).
   * If regression > `_regression_threshold_pct` — log current,
     fail with `::error::`.
   * Otherwise — log current, pass.
4. Writes a Markdown summary to `$GITHUB_STEP_SUMMARY` showing
   current vs baseline vs delta.

## How to regenerate baselines

Baselines should be regenerated when:

1. A benchmark is intentionally made slower (explanation in the
   commit message + PR description).
2. A benchmark is removed or renamed.
3. A minor version bump — per TESTING-TAXONOMY.adoc, baselines are
   reset at minor-version boundaries.

Two ways to regenerate:

### 1. Locally (recommended for one-off tweaks)

```bash
cargo bench --bench hypatia_bench -- \
  --warm-up-time 1 --measurement-time 2 --sample-size 10 \
  --output-format bencher \
  | tee /tmp/bench.txt

# Parse the output and update baselines.json:
python3 scripts/update-bench-baselines.py /tmp/bench.txt \
  .machine_readable/benchmarks/baselines.json
```

Review the diff, commit, push.

### 2. Workflow dispatch

Run the `Benchmarks` workflow manually with input
`mode: regenerate-baseline`. It runs the full suite, writes new
baselines.json content to `$GITHUB_STEP_SUMMARY`, and uploads a
patched file as an artefact for a human to download, eyeball, and
commit.

The workflow deliberately does NOT auto-commit — baseline changes
are meaningful and should be reviewed, not silently adopted.

## When is the first baseline written?

Until `baselines.json` has a non-empty `baselines` key, every PR
run passes the comparison step (it has nothing to compare to).
Current numbers are still logged in the run summary, so the job is
useful as an observatory even without gating.

Seed the first baseline by running the regenerate flow on main
after the CI job's been green long enough that the numbers feel
representative.

## What is NOT in baselines

Only steady-state latency / throughput numbers belong here. The
Six Sigma taxonomy also calls for memory, startup, build-time,
energy, and FFI-overhead benchmarks — those are absent for now
and tracked as the remainder of punch-list item #1 in
`TESTING-AUDIT.md`.
