# CLAUDE.md - Hypatia AI Assistant Instructions

## Project Overview

Hypatia is the neurosymbolic CI/CD intelligence layer for the hyperpolymath ecosystem. It coordinates the gitbot-fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot) via a safety triangle pipeline, with Logtalk rules for pattern detection and ArangoDB planned for knowledge graph storage.

## Architecture

```
Hypatia
├── Elixir pipeline          # 8 core modules (pattern analysis, dispatch, learning)
├── Logtalk rule engine       # Error catalog, pattern detection rules
├── Rust workspace            # adapters, cli, data, fixer, integration
├── Safety triangle           # Eliminate > Substitute > Control
├── Fleet dispatcher          # Routes findings to appropriate bots
└── Integration connectors    # verisimdb-data, panic-attack, gitbot-fleet
```

## Key Commands

```bash
mix deps.get    # Install Elixir deps
mix test        # Run tests
mix format      # Format Elixir code
cargo build     # Build Rust workspace
cargo test      # Test Rust workspace
```

## Machine-Readable Artefacts

Files in `.machine_readable/` contain structured project metadata:

- `STATE.scm` - Current project state and progress
- `META.scm` - Architecture decisions and development practices
- `ECOSYSTEM.scm` - Position in the ecosystem and related projects
- `AGENTIC.scm` - AI agent interaction patterns
- `NEUROSYM.scm` - Neurosymbolic integration config
- `PLAYBOOK.scm` - Operational runbook

## Safety Triangle Pipeline (OPERATIONAL)

### Data Flow

```
panic-attack assail (scan repos)
        | JSON results
verisimdb-data repo (git-backed flat-file store, 292 repos scanned)
        | read scan results
Elixir pipeline:
  VerisimdbConnector.fetch_all_scans()
        |
  PatternRegistry.sync_from_scans()     -- 954 canonical patterns
        |
  TriangleRouter.route()                -- Eliminate > Substitute > Control
        |
  FleetDispatcher.dispatch_routed_action()
        |
  DispatchManifest.write()              -- JSONL for execution layer
        |
dispatch-runner.sh (gitbot-fleet)
  ├── auto_execute (>=0.95 confidence): robot-repo-automaton
  ├── review (0.85-0.94): rhodibot creates PR
  └── report_only (<0.85): sustainabot advisory
        |
OutcomeTracker.record_outcome()         -- Feedback loop
```

### Core Elixir Modules (lib/)

| Module | Purpose |
|--------|---------|
| `pattern_analyzer.ex` | Full pipeline orchestrator: scan -> patterns -> triangle -> dispatch |
| `verisimdb_connector.ex` | Reads scans, patterns, recipes from verisimdb-data |
| `pattern_registry.ex` | Deduplicates findings into canonical patterns (PA001-PA020) |
| `recipe_matcher.ex` | Fuzzy matching: fingerprinted IDs to clean recipe IDs |
| `triangle_router.ex` | Routes through Eliminate > Substitute > Control hierarchy |
| `fleet_dispatcher.ex` | Confidence-gated dispatch to fleet bots |
| `dispatch_manifest.ex` | Writes JSONL manifests as bridge to bash execution |
| `outcome_tracker.ex` | Records fix outcomes, updates recipe confidence |

### Metrics (as of 2026-02-13)

- 385 auto_execute fixes dispatched (0 failures)
- 86.3% weak point reduction: 3260 -> 447
- 282 outcomes recorded
- 6 recipes at 0.99 confidence
- 10 fix recipes total (12 recipe files)
- 954 canonical patterns across 292 repos

### Remaining Work (M4: Production Operations)

- GraphQL API for live fleet coordination (currently stub-only)
- ArangoDB knowledge graph storage
- Automated re-scanning and trend detection
- SARIF output for IDE integration
- PAT required for automated cross-repo dispatch

## Code Style

- Elixir: `mix format`, SPDX headers on all files
- Rust: `rustfmt`, `clippy`
- Logtalk: Follow coding guidelines
- Shell: `ShellCheck`, POSIX-compatible
- SPDX-License-Identifier: PMPL-1.0-or-later

## Security Requirements

- No MD5/SHA1 for security (use SHA256+)
- HTTPS only (no HTTP URLs)
- No hardcoded secrets
- SHA-pinned dependencies
- SPDX license headers on all files
