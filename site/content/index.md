---
title: Hypatia
description: Neurosymbolic CI/CD intelligence platform for multi-forge software ecosystems
date: 2026-03-02
license: PMPL-1.0-or-later
language: en
---

# Hypatia

**Neurosymbolic CI/CD intelligence that understands your code across every forge.**

Hypatia is a next-generation security and quality scanning platform that combines neural network pattern recognition with symbolic rule evaluation. It connects to GitHub, GitLab, Bitbucket, Codeberg, SourceHut, and Radicle — scanning repositories, detecting vulnerabilities, and dispatching automated fixes through the gitbot-fleet.

## Architecture

Hypatia is a polyglot system built from purpose-matched languages:

| Layer | Language | Purpose |
|-------|----------|---------|
| **Neural Engine** | Elixir/OTP | 5 neural networks for pattern recognition, fault-tolerant supervision |
| **Forge Adapters** | Rust | 6 forge adapters with webhook verification, connection pooling |
| **Rule Engine** | Elixir | Declarative security and quality rules with caching |
| **Formal ABI** | Idris2 | Dependent-type verified interface definitions |
| **C Bridge** | Zig | Zero-overhead FFI with truncation detection |
| **Data Layer** | Rust | ArangoDB (graph storage) + Dragonfly (caching, pub/sub) |
| **CLI** | Rust | `hyper` command-line interface for local scanning |

## Neural Networks

Hypatia runs five specialised neural networks:

- **PatternNet** — Detects anti-patterns, code smells, and style violations
- **VulnNet** — Identifies security vulnerabilities and CVE patterns
- **DependencyNet** — Analyses dependency graphs for supply-chain risks
- **ComplianceNet** — Checks license compliance and regulatory requirements
- **TrendNet** — Tracks quality trends and predicts degradation

## Forge Support

| Forge | Status | Webhooks | API |
|-------|--------|----------|-----|
| GitHub | Full | HMAC-SHA256 | REST v3 |
| GitLab | Full | Token comparison | REST v4 |
| Bitbucket | Full | IP allowlist | REST 2.0 |
| Codeberg/Gitea | Full | HMAC-SHA256 | REST v1 |
| SourceHut | Full | HMAC-SHA256 | REST + GraphQL |
| Radicle | Full | Local node | HTTP API |

## Ecosystem Integration

Hypatia is the scanning intelligence layer in the hyperpolymath reposystem:

- **gitbot-fleet** — 6 specialised bots receive Hypatia findings and act on them
- **robot-repo-automaton** — Automated fix commits with confidence thresholds
- **proven-servers** — Formally verified protocol implementations for secure comms
- **VeriSimDB** — Graph/vector/temporal database backing the data layer
- **PanLL** — Mission control dashboard (Hypatia powers the Panel-N reasoning layer)

## Quick Start

```bash
# Clone
git clone https://github.com/hyperpolymath/hypatia.git
cd hypatia

# Build all Rust components
cargo build --release

# Run CLI scan
./target/release/hyper scan --repo https://github.com/owner/repo

# Start the engine (requires Elixir, ArangoDB, Dragonfly)
cd engine && mix deps.get && mix phx.server
```

## Project Status

Hypatia is in active development. The Rust adapter layer is production-ready with full test coverage. The Elixir neural engine and rule system are functional. The data layer (ArangoDB + Dragonfly) requires deployment infrastructure.

## License

PMPL-1.0-or-later (Palimpsest License)

Built by [Jonathan D.A. Jewell](https://github.com/hyperpolymath) as part of the hyperpolymath software ecosystem.
