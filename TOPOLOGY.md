<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# Hypatia — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              FORGE ADAPTERS             │
                        │      (GitHub, GitLab, sr.ht, etc.)      │
                        └───────────────────┬─────────────────────┘
                                            │ Action Layer
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           HYPATIA PLATFORM              │
                        │                                         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │Verification│  │  Logtalk Rule     │  │
                        │  │Registry    │  │  Engine           │  │
                        │  │ (Haskell)  │  │  (Symbolic)       │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        │        │                 │              │
                        │  ┌─────▼─────┐  ┌────────▼──────────┐  │
                        │  │ Neural    │  │  Action Layer     │  │
                        │  │ Learning  │  │ (robot-repo-auto) │  │
                        │  │ (Pattern) │  │                   │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │             DATA LAYER                  │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ ArangoDB  │  │ Dragonfly │  │ Radicle││
                        │  │ (Graph)   │  │ (Cache)   │  │ (P2P)  ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile / mix.exs .machine_readable/  │
                        │  K8s / Helm         0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE PLATFORM
  Logtalk Rule Engine               ██████████ 100%    Symbolic reasoning stable
  Haskell Ruleset DSL               ██████████ 100%    Type-safe registry active
  Verification (Liquid Haskell)     ██████████ 100%    90% tasks verified
  Neural Learning Layer             ████████░░  80%    Rule distillation refining

INTERFACES & ADAPTERS
  Forge Adapters (4/6)              ██████████ 100%    GH/GL/BB/CB stable
  CLI Interface (hyper)             ██████████ 100%    Full scan capability verified
  robot-repo-auto Connector         ██████████ 100%    Automation hooks active

DATA & DEPLOY
  ArangoDB Graph Schema             ██████████ 100%    Repo/Rule relationships stable
  Dragonfly Cache                   ██████████ 100%    Sub-ms lookups verified
  Kubernetes / Helm                 ██████████ 100%    Production deploy ready

REPO INFRASTRUCTURE
  Justfile Automation               ██████████ 100%    Standard build/lint/test
  .machine_readable/                ██████████ 100%    STATE tracking active
  OPSM Integration                  ██████████ 100%    Policy services verified

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██████████ 100%    Phase 3 Complete, Scaling v1.0
```

## Key Dependencies

```
Neural Pattern ───► Rule Distiller ───► Haskell Verify ──► Registry
     │                 │                   │                │
     ▼                 ▼                   ▼                ▼
Fail Context ────► Logtalk Engine ────► Action Hook ───► Forge Sync
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
