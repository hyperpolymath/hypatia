<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

# docs/architecture/

Architecture documentation for the hypatia neurosymbolic CI/CD intelligence platform.

## Contents

| Document | Purpose |
|----------|---------|
| [togaf-overview.adoc](togaf-overview.adoc) | TOGAF-style as-built architecture overview (verified by code audit, 2026-05-18). Canonical. |
| [mof-metamodel.adoc](mof-metamodel.adoc) | OMG MOF M2 metamodel of the core domain (Scan, Finding, Pattern, Recipe, RoutedAction, Outcome, OctadEntity, NeuralOrgan, …). Canonical. |
| [boundary-design-options.adoc](boundary-design-options.adoc) | Verisimdb-via-SNIF boundary: three options (A split / B WASI / C no SNIF) — **OPEN, awaiting owner ruling** ([#294](https://github.com/hyperpolymath/hypatia/issues/294)). |
| [NEURAL-ARCHITECTURE.md](NEURAL-ARCHITECTURE.md) | Neural subsystem detail: 8-organ blackboard, 6 phases, training, persistence. |

> Legacy Mermaid diagrams under `docs/diagrams/*.md` are **superseded** by the canonical `.adoc` documents above.

## See Also

- [TOPOLOGY.md](../../TOPOLOGY.md) — High-level architecture diagram and completion dashboard
- [.machine_readable/STATE.scm](../../.machine_readable/STATE.scm) — Current project state
- [.claude/CLAUDE.md](../../.claude/CLAUDE.md) — AI agent instructions
