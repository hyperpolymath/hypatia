<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Operations

Running Hypatia in anger: the mix tasks, the HTTP surfaces, the environment, and what to do
when something breaks.

## Mix tasks

All 15 shipped tasks (`mix help | grep hypatia`):

| Task | What it does |
|---|---|
| `mix hypatia.rsr_score` | Score a repo tree against the RSR v2.0 criteria SSOT |
| `mix hypatia.recipe_health` | Per-recipe verification rates, quarantine candidates, degraded recipes |
| `mix hypatia.watch` | Live watcher — counters, queue depths, recent events |
| `mix hypatia.triage_issues` | Triage scan findings into issue submissions |
| `mix hypatia.merge_orchestrate` | Drive the merge-orchestration tier |
| `mix hypatia.validate_leases` | Validate merge-orchestration leases |
| `mix hypatia.pr_eligibility` | Decide whether a PR is eligible for automated handling |
| `mix hypatia.record_outcome` | Record a fix outcome (drives Bayesian confidence updating) |
| `mix hypatia.reconcile` | Reconcile scanner state against the store |
| `mix hypatia.audit_repos` | Estate-wide repo audit |
| `mix hypatia.batch_security_scan` | Batch security scan across repos |
| `mix hypatia.repo_batch_ops` | Bulk repository operations |
| `mix hypatia.deploy_prevention_workflows` | Push prevention workflows to consumers |
| `mix hypatia.strategy_effectiveness` | Compare neural rebalancer strategies A/B/C |
| `mix hypatia.verify_action_shas` | Verify GitHub Action SHA pins resolve |

**Known bug:** `mix hypatia.rsr_score` cannot find its own criteria SSOT by default — the
default path resolves inside `_build`. Pass it explicitly until that is fixed:

```bash
mix hypatia.rsr_score . --ssot test/fixtures/a2ml/rsr-criteria-v2.a2ml
```

## HTTP surfaces

The supervision tree starts Bandit on port 9090 (9099 under `MIX_ENV=test`):

| Path | Purpose |
|---|---|
| `/` | HTML dashboard (live event stream) |
| `/api/status` | Watcher snapshot — counters, queue depths, uptime |
| `/api/counts/:window` | Event counts for `5m` / `1h` / `1d` |
| `/api/recipes` | Recipe health |
| `/api/events` | Server-sent events stream |
| `/api/alerts` | Alert history |
| `/api/quarantine` | Quarantined bots |
| `/metrics` | Prometheus exposition format |

`/api/*` is bearer-token authenticated.

## Environment variables

| Variable | Purpose |
|---|---|
| `HYPATIA_DISPATCH_PAT` | GitHub PAT with `repo` scope for cross-repo dispatch |
| `HYPATIA_HTTP_PORT` | Override the Bandit listen port (default 9090) |
| `HYPATIA_VERISIM_URL` | verisim-api endpoint, when deployed |
| `HYPATIA_FLEET_PATH` | Path to the gitbot-fleet checkout |
| `HYPATIA_ALERT_WEBHOOK_URL` | Webhook for watcher alerts |
| `HYPATIA_ALERT_LOG_FILE` | File sink for watcher alerts |
| `HYPATIA_EXIT_ZERO` | Make the CLI exit 0 even when findings are present |

## CLI

The escript supports exactly five commands — `scan`, `report`, `pr-eligibility`, `version`,
`help`. Anything else exits with "unknown command".

```bash
hypatia scan <path> [--format json|text|github|sarif] [--exit-zero]
hypatia report <path>
hypatia pr-eligibility --owner <o> --repo <r> --pr <n>
```

## Incident response

1. Check `/api/status` (or `mix hypatia.watch`) for queue depth and dropped events.
2. Check quarantine: `/api/quarantine`. A bot at `hard` has hit 5 consecutive failures.
3. Check the circuit breaker in `Hypatia.SelfDiagnostics`.
4. If dispatch is misbehaving, the safety systems are the first thing to verify — **note that
   the rate limiter, quarantine and batch rollback are currently built but not invoked by any
   dispatch path**, so they will not save you. See `docs/DEBT-REGISTER.md`.
5. Roll back a bad batch with `Hypatia.Safety.BatchRollback` (manual invocation).

## Release

1. Bump the version in `mix.exs`.
2. Update `CHANGELOG.md`.
3. Tag and push; the release workflow builds containers and publishes.

**Caveat:** `release.yml` references `workflow_dispatch` inputs it never declares, so its
dry-run guard is permanently falsy — every dispatch publishes for real (issue #637).

## Health of the gates themselves

Before trusting a green board, know what is *not* gated:

- 242 `:verisim_data` tests are excluded from every run and fail 129/242 when included.
- No Elixir lint gate (`mix compile --warnings-as-errors` currently fails with 21 warnings).
- No `zig build` in CI, so the 18 FFI exports and 16 connectors are unverified.
- Two workflows (`clusterfuzzlite.yml`, `security-policy.yml`) produce zero jobs on every push.

The full picture is `docs/DEBT-REGISTER.md`.
