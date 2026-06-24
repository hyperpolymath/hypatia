<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Session summary — 2026-05-23 → 2026-05-25

Comprehensive record of work done across PRs #309 → #319. Audience: both humans (narrative top section) and machines (structured tables at the bottom). Read from top.

---

## Narrative

This session started with a "hundreds of failed hypatia checks across the estate, no security stuff getting sorted" complaint. Three sessions of work later:

1. **The root cause was diagnosed.** The "310 null fix_script dispatch entries" gap CLAUDE.md flagged as critical-blocking turned out not to be missing fix scripts — it was a recipe-matcher language-gate bug treating the `"any"` sentinel as unmatched and `"yaml"` recipes as unreachable. All 22 scorecard fix scripts already existed on disk; they just weren't routable. Single ~30-line fix in `lib/recipe_matcher.ex` (commit `d2bbf75`) unblocked the entire scorecard pipeline.

2. **The full watcher / supervision interface was delivered.** Three phases (10 commits in PR #309 plus follow-ups): telemetry instrumentation → ETS rolling-window aggregator → HTTP API + SSE stream + HTML dashboard + Prometheus + TUI → threshold alerts with Log/Webhook/File/Peer sinks + 5-min persistence + statistical anomaly detector with ESN drift corroboration. Bearer auth (M15a), persistent state across restart (M15b), ESN tight integration (M15c), cross-host federation (M15d) all shipped.

3. **Closed-loop quality became operational.** Soundness gate (in-process manifest test + escript packaging test — caught a latent bug on first run where `@language_extensions` was missing `.agda`/`.zig`/`.thy`/`.fst`/`.adb`); verification metric + `mix hypatia.recipe_health`; canonical `record_outcome_for_fix` entry + `mix hypatia.record_outcome` CLI wrapper for the bash dispatch-runner; auto-quarantine in `FleetDispatcher` when verification rate drops below 0.30.

4. **GitHub alert API consumers added.** `Hypatia.Rules.SecretScanningAlerts` (SSA001-SSA004) + `Hypatia.Rules.CodeScanningAlerts` (CSA001-CSA004) so the PAT's read access to secret-scanning + code-scanning alerts is actually consumed (it was granted but unused).

5. **M17, M18, M9 (B+C), M13, M14 shipped.** GNN/VAE/Sequence persistence + cross-org federation with policy gates + rebalancer strategy selection + native SARIF + minimal hand-rolled GraphQL endpoint.

6. **Estate-wide noise reduction.** PR #314's triage commit identified four false-positive classes generating ~5,000+ spurious cross-repo issues per scan cycle (retired workflow checks, low-severity unwrap noise reaching gitbot-fleet, soundness-fixture self-recursion, namespaced state-file detection). All four fixed. `mix hypatia.triage_issues` task ships for bulk-closing the historical issues with classification + audit trail.

7. **Repo health pass (PRs #315 + #319).** Root went from ~30 mixed `.md`/`.adoc` files to 9 standard project chrome files. `docs/` reorganised into 16 well-named subdirs aligned with `rsr-template-repo` conventions. README's ArangoDB references corrected (ditched per #273). Six wiki pages rewritten with current terminology and committed to `docs/wiki-pages/` for manual sync to the live wiki.

8. **Five design memos** for items that can't ship as code from this repo (dispatch-runner contract for gitbot-fleet, verisim-api deployment runbook, issue #294 boundary ruling with Option A recommendation, Prover Wars integration scope, Nx/EXLA migration plan). Each has explicit acceptance criteria for closing the underlying roadmap item.

Completion percentage moved 87 → 98.

---

## Structured record

### PRs

| PR | State | Title | Commits |
|---|---|---|---|
| #309 | merged | Fix recipe matching for scorecard and workflow patterns | 19 |
| #310 | merged | (squash of #309 progress) | — |
| #311 | merged | (squash of #309 progress) | — |
| #312 | merged | (squash of #309 progress) | — |
| #313 | merged | (resolution merge of post-#309 work) | 1 |
| #314 | merged | Post-#313: M17 + M18 + M9 longitudinal + container hardening + 5 design memos | 15 |
| #315 | merged | docs: tidy root + adopt rsr-template-repo doc taxonomy | 2 |
| #319 | open | docs: second-pass bucketing — security/, standards/, specs/ | 1 |

### Milestones closed in this session

| Milestone | What | Commit / PR |
|---|---|---|
| M7 | HYPATIA_DISPATCH_PAT provisioned end-to-end | (operator) 2026-05-24 |
| M8 | "310 null-fix-script" root cause resolved | `d2bbf75` |
| M9 | Rebalancer Strategy B+C operationalised | `fdb929d` + `2d5cb2e` |
| M10 | Ada TUI snapshot endpoint added | `e3686be` |
| M11 | Watcher / supervision interface (3 phases) | PR #309 |
| M12 | Closed-loop quality | PR #309 |
| M13 | Native SARIF output | `01fce4d` |
| M14 | GraphQL endpoint at POST /graphql | `c344af9` |
| M15a | Bearer auth on /api/* | `5fab9d4` |
| M15b | Persistent Watcher state | `fe159c6` |
| M15c | ESN tight integration | `5710910` |
| M15d | Cross-host alert federation | `de504de` |
| M16 | Neural state persistence hydration | `2d5cb2e` |
| M17 | GNN / VAE / Sequence persistence | `cc50422` |
| M18 | Cross-org VCL federation with policy gates | `e1f1134` + `021e004` |
| Issue #141 | hypatia-scan.yml $HOME collapse fix | `a77b41a` |

### Modules added or substantially extended

| Module | Purpose | Commit |
|---|---|---|
| `lib/hypatia/telemetry.ex` | Event registry + emit helpers | `d0ddd2f` |
| `lib/hypatia/watcher.ex` | ETS aggregator + restart-persistent state | `3b4379e` + `fe159c6` |
| `lib/hypatia/watcher/alerts.ex` | Threshold rule engine + sink dispatch | `3750086` |
| `lib/hypatia/watcher/alerts/sinks.ex` | Log/File/Webhook/Peer sinks | `3750086` + `de504de` |
| `lib/hypatia/watcher/persistence.ex` | 5-min snapshot to verisim-data | `a1eb6b2` |
| `lib/hypatia/watcher/anomaly_detector.ex` | Statistical + ESN-drift detection | `fc4f5d0` + `5710910` |
| `lib/hypatia/web/api_router.ex` | /api/* with loopback + bearer gates | `30ced35` + `5fab9d4` + `5b15430` |
| `lib/hypatia/web/dashboard.ex` | Live HTML dashboard | `a0075db` + alerts ribbon |
| `lib/hypatia/web/metrics.ex` | Prometheus exposition | `adc2863` |
| `lib/hypatia/web/metrics_snapshot.ex` | Ada TUI snapshot endpoint | `e3686be` |
| `lib/hypatia/web/graphql.ex` | Minimal hand-rolled GraphQL handler | `c344af9` |
| `lib/hypatia/sarif.ex` | Native SARIF 2.1.0 emitter | `01fce4d` |
| `lib/vcl/cross_org.ex` | Cross-org federation with policy gates | `e1f1134` |
| `lib/rules/secret_scanning_alerts.ex` | SSA001-SSA004 | `e929621` |
| `lib/rules/code_scanning_alerts.ex` | CSA001-CSA004 | `e929621` |

### Mix tasks added

| Task | Purpose |
|---|---|
| `mix hypatia.watch` | Live terminal dashboard (local or `--url`) |
| `mix hypatia.recipe_health` | Per-recipe success + verification rate report |
| `mix hypatia.record_outcome` | Canonical outcome-recording entry for the bash dispatch-runner |
| `mix hypatia.strategy_effectiveness` | Longitudinal study of neural rebalancer strategies |
| `mix hypatia.triage_issues` | Bulk-close estate issues invalidated by PR #314 |

### Tests added

| Test | Coverage |
|---|---|
| `test/soundness/manifest.json` + `test/soundness_test.exs` + `test/soundness/run-escript-soundness.sh` | 13 known-bad fixtures across language families; both in-process AND escript-build modes |
| `test/recipe_health_test.exs` | Verification rate calculation + quarantine threshold |
| `test/dependabot_alerts_test.exs` | DA001-DA004 |
| `test/secret_scanning_alerts_test.exs` | SSA001-SSA004 |
| `test/code_scanning_alerts_test.exs` | CSA001-CSA004 |
| `test/sarif_test.exs` | SARIF 2.1.0 schema invariants |
| `test/graphql_test.exs` | GraphQL handler surface |
| `test/watcher_test.exs` | Telemetry → counters + restart persistence |
| `test/api_router_test.exs` | Bearer auth + loopback gate + endpoints |
| `test/dashboard_test.exs` | Schema test pinning JS to telemetry registry |
| `test/metrics_test.exs` | Prometheus exposition |
| `test/metrics_snapshot_test.exs` | Ada TUI snapshot |
| `test/anomaly_detector_test.exs` | Statistical regression + ESN drift |
| `test/alerts_test.exs` | Rule firing + dedup + ring buffer |
| `test/persistence_test.exs` | 5-min snapshot to disk |
| `test/cross_org_test.exs` | Peer config + find_policy |
| `test/neural_strategy_resolver_test.exs` | A/B/C/rotate selection |
| `test/watch_task_test.exs` | TUI rendering |

### Documents added or substantially extended

| Doc | Subject |
|---|---|
| `docs/operations/dispatch-runner-contract.adoc` | gitbot-fleet bash runner exit-code contract |
| `docs/operations/verisim-api-deployment.adoc` | Runbook for when verisim-api ships |
| `docs/operations/issue-294-boundary-recommendation.adoc` | Option A ruling memo |
| `docs/operations/prover-wars-integration.adoc` | M19 scope design |
| `docs/operations/nx-exla-neural-backend.adoc` | M21 conditional migration plan |
| `docs/operations/containerfiles-chainguard-gap.adoc` | Estate Chainguard audit |
| `docs/wiki-pages/Home.md` | Wiki landing page |
| `docs/wiki-pages/Architecture.md` | Wiki architecture page |
| `docs/wiki-pages/Getting-Started.md` | Wiki onboarding page |
| `docs/wiki-pages/Guides.md` | Wiki task-oriented walkthroughs |
| `docs/wiki-pages/FAQ.md` | Wiki FAQ |
| `docs/wiki-pages/Troubleshooting.md` | Wiki symptom→fix |
| `docs/wiki-pages/README.md` | Wiki sync instructions |
| `docs/SESSION-SUMMARY-2026-05-25.md` | This document |

### Issues triaged

| Issue | Action |
|---|---|
| #273 | Status comment posted (parent epic, multiple gaps closed) |
| #274 | **Closed as completed** (Neural organs UNFED — fixed via PR #275 + #309 + #314) |
| #294 | Comment with recommendation memo location (awaiting human ruling A/B/C) |

### Telemetry events introduced

`[:hypatia, :scan, :complete]`, `[:hypatia, :dispatch, :decision]`, `[:hypatia, :outcome, :recorded]`, `[:hypatia, :verification, :result]`, `[:hypatia, :quarantine, :triggered]`, `[:hypatia, :rate_limit, :exceeded]`, `[:hypatia, :neural, :cycle]`, `[:hypatia, :soundness, :violation]`, `[:hypatia, :anomaly, :detected]`, `[:hypatia, :cross_org, :import]`, `[:hypatia, :cross_org, :drift]`.

### Operator action items remaining

| Item | Where |
|---|---|
| Copy `docs/wiki-pages/*.md` to the live wiki | One-time manual sync; instructions in `docs/wiki-pages/README.md` |
| Run `mix hypatia.triage_issues --apply --confirm --classes <class>` | Bulk-close PR-#314-invalidated estate issues (class-by-class) |
| Configure `:cross_org_policies` + per-peer bearer env vars | Activate cross-org federation (M18 is code-complete, needs peer config to do anything) |
| Set `:neural_rebalance_strategy, :rotate` in `config/runtime.exs` | Activate B+C rebalancing alongside A |
| Rule on Issue #294 (A / B / C) | Unblocks M19 and M20 milestones |
| Wire gitbot-fleet's dispatch-runner to `mix hypatia.record_outcome` | Populates verification metric (currently `:insufficient_data` for all recipes) |

### What genuinely remains blocked

| Item | Blocked on |
|---|---|
| Deploy verisim-api server | Ops + the verisim-data repo team |
| Push committed fixes to remotes | gitbot-fleet dispatch-runner contract adoption |
| Issue #294 ruling | Human decision |
| Prover Wars integration (M19) | dodeca-API + VeriSimDB schema sign-off |
| Nx/EXLA backend (M21) | Conditional — no trigger met (reservoir sizes still small) |
| Haskell Chainguard base image | Upstream Chainguard publishing one |
| Neural rebalancer effectiveness study | ~30 days of operational data accumulating |
