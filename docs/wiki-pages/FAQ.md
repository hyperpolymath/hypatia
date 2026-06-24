<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# FAQ

## What is Hypatia?

A neurosymbolic CI/CD intelligence platform. It scans the hyperpolymath estate (~290 repos), detects security and policy issues, and dispatches fixes through the gitbot-fleet. Findings flow through a safety triangle (eliminate → substitute → control); confidence-gated routing decides whether a fix auto-executes, opens a PR for review, or surfaces as an advisory.

## What does "neurosymbolic" mean here?

Two layers cooperate:
- **Symbolic** — Elixir rule modules that pattern-match against code, recipes that name fixes, a triangle router that selects strategies.
- **Neural** — 8 networks (RBF, PageRank, GNN, MoE, ESN, LSM, VAE, Sequence) running on a 6-phase blackboard. They learn from outcomes and adjust confidence.

Neither layer alone is the system. The rules give determinism + auditability; the networks give learning + adaptation.

## Where did VQL go?

Renamed to **VCL** (Verisim Conditional Language) on 2026-04-05. Same syntax. The CHANGELOG entry explains the rename rationale.

## I thought you used ArangoDB?

The architecture audit (#273, gap 1) found ArangoDB framed as the storage layer but it had been ditched. The canonical store is **verisim-data** — a git-backed flat-file repo queried via the in-process VCL parser + `FileExecutor`. The README, wiki, and STATE.a2ml have all been corrected.

## Why low-severity findings sometimes don't reach my repo

PR #314's noise-reduction batch (2026-05-25) added a `medium+` filter on Phase 2 submission to gitbot-fleet. Low-severity findings stay on the SARIF advisory page + build artifact, never cross to sustainabot. This eliminated ~3,000 false-positive cross-repo issues per scan cycle.

## A finding I think is wrong shows up. What now?

Three suppression mechanisms in priority order:

1. **Fix the code.** Often easier than you'd think.
2. **Inline directive** at the call site with a reason:
   ```rust
   let pw = "x"; // hypatia: allow security_errors/secret_detected -- doctest
   ```
3. **`.hypatia-ignore`** for file-scoped or directory-scoped exemptions with documented rationale.

`.hypatia-baseline.json` is **last resort** — every agent reads baseline entries as historical risk.

## How does auto-quarantine work?

`Hypatia.OutcomeTracker.quarantined?/2` returns `true` when a recipe's verification rate drops below `:threshold` (default 0.30) over `:min_attempts` (default 5) verifiable outcomes. `FleetDispatcher` checks this before every `:auto_execute` and downgrades to `:review` if true.

Set `HYPATIA_RECIPE_QUARANTINE_DISABLE=true` to bypass in emergencies (logged on every consultation).

## What runs every 5 minutes?

`Hypatia.LearningScheduler`'s tick. Each cycle:
1. Ingests new outcomes from local + fleet logs
2. Bayesian-updates affected recipes' confidence
3. Reports confidence drift + annealing state
4. Detects strategy-shift events + re-queues
5. Retrains the prover recommender from VeriSimDB
6. Triggers a Neural Coordinator force_cycle (all 8 networks)
7. Polls cross-org peers (if `:cross_org_policies` is configured)

## What runs every 5 seconds / 30 seconds?

- **Every 5 s** — `Watcher.poll_queues` (GenServer mailbox lengths)
- **Every 5 s** — dashboard `/api/status` poll
- **Every 30 s** — `Watcher.Alerts.tick` (threshold-rule evaluation)
- **Every 60 s** — `Watcher.AnomalyDetector.tick` (statistical + ESN drift)
- **Every 60 s** — `Watcher.persist` (state file flush)
- **Every 5 min** — `Watcher.Persistence` snapshot to verisim-data

## Daily cron jobs

| Job | Cadence | Purpose |
|---|---|---|
| `hypatia-remediation-sweep.yml` | 03:17 UTC daily | Org-wide vulnerability sweep → repository_dispatch to gitbot-fleet |
| `hypatia-scan.yml` | Weekly (Sun 00:00 UTC) + on push/PR | Per-repo scan with SARIF upload |

## What's MPL-2.0 vs PMPL-1.0?

- **MPL-2.0** is the SPDX-identifiable license declared on every file.
- **PMPL-1.0-or-later** (Palimpsest profile of MPL) is the philosophy on top. PALIMPSEST.adoc has the rationale.

Both stay in sync — PMPL is an MPL-compatible profile.

## What about the Ada TUI?

`tui/` contains an Ada TUI. It compiles. `Hypatia.TUI.Port` (in `lib/tui/port.ex`) is supervised conditionally on `:hypatia, :tui_enabled` (default false). When enabled, it polls `/metrics/snapshot` every 10s and sends JSON to the Ada process. Set `:tui_enabled: true` in `config/runtime.exs` to activate.

## How do I see what's currently auto-quarantined?

```bash
mix hypatia.recipe_health --only-actionable
# OR via HTTP
curl -H "Authorization: Bearer $HYPATIA_API_BEARER_TOKEN" \
     http://localhost:9090/api/quarantine
```

## Open architecture decisions

- **Issue #294** — VCL / SNIF / verisimdb boundary (A / B / C ruling). Recommendation memo at `docs/operations/issue-294-boundary-recommendation.adoc` argues for Option A.

## Where do I get help?

- Issues: <https://github.com/hyperpolymath/hypatia/issues>
- Architecture docs: [`docs/architecture/`](https://github.com/hyperpolymath/hypatia/blob/main/docs/architecture/)
- Roadmap: [`ROADMAP.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/ROADMAP.adoc)
- Live status (if you run it): `http://localhost:9090/`
