<!-- SPDX-License-Identifier: MPL-2.0 -->
# Hypatia

**Hypatia is the neurosymbolic CI/CD intelligence layer for the hyperpolymath ecosystem.**
It coordinates the gitbot-fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot, panicbot) via a safety-triangle pipeline, with 8 neural networks running on a 6-phase blackboard, a VCL query layer over a git-backed flat-file store, Bayesian confidence updating, and Elixir rules for pattern detection.

## Quick navigation

- **[Getting Started](Getting-Started)** — install + run your first scan in 5 minutes.
- **[Architecture](Architecture)** — components, data flow, neural blackboard.
- **[Guides](Guides)** — task-oriented walkthroughs.
- **[FAQ](FAQ)** — common questions.
- **[Troubleshooting](Troubleshooting)** — when things go wrong.

## Key concepts

- **Safety triangle.** Every finding routes through **Eliminate → Substitute → Control** (in that priority order). Hypatia tries hardest to eliminate root causes first; failing that, substitute with a proven-safe alternative; failing that, control via human-reviewed advisory.
- **Confidence-gated routing.** Dispatches gate on Bayesian-updated recipe confidence:
  - `:auto_execute` at ≥ 0.95 → robot-repo-automaton commits the fix
  - `:review` at 0.85 – 0.94 → rhodibot opens a PR for human review
  - `:report_only` < 0.85 → sustainabot writes an advisory issue
- **Closed-loop verification.** After every fix, panic-attack re-scans the repo. If the finding persists, the dispatch is auto-recorded as `:false_positive` and the recipe's confidence drops. Recipes that drift below 0.30 verification rate over ≥ 5 verifiable outcomes get auto-quarantined.
- **VCL.** A small SQL-like query language over the canonical verisim-data flat-file store. Use it for cross-repo analytics, federation queries, and historical trend reads.
- **Blackboard neural architecture.** 8 networks (Graph-of-Trust PageRank, MoE, LSM, ESN, RBF, GNN, VAE, Sequence) read and write to a shared ETS blackboard across 6 execution phases. No fixed network weights — confidence derives from the full reasoning trace.

## In-tree documentation index

See [`docs/README.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/docs/README.adoc) for the authoritative documentation index. Layout follows `rsr-template-repo` conventions.

## Status

Current milestones tracked in [`ROADMAP.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/ROADMAP.adoc). Component grades in [`docs/readiness.md`](https://github.com/hyperpolymath/hypatia/blob/main/docs/readiness.md).

## Where to get help

- Issues: <https://github.com/hyperpolymath/hypatia/issues>
- Site: <https://hypatia.reposystem.dev>
- License: PMPL-1.0-or-later (Palimpsest profile of MPL-2.0); see [`PALIMPSEST.adoc`](https://github.com/hyperpolymath/hypatia/blob/main/PALIMPSEST.adoc).
