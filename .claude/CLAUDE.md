# CLAUDE.md - Hypatia AI Assistant Instructions

## Project Overview

Hypatia is the neurosymbolic CI/CD intelligence layer for the hyperpolymath ecosystem. It coordinates the gitbot-fleet (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot) using Logtalk rules, ArangoDB for knowledge storage, and a GraphQL API.

## Architecture

```
Hypatia
├── Logtalk rule engine       # Pattern detection, decision rules
├── ArangoDB                  # Knowledge graph storage
├── GraphQL API               # Fleet coordination interface
├── Fleet dispatcher          # Routes findings to appropriate bots
└── Integration connectors    # verisimdb, panic-attack, hardware-crash-team
```

## Machine-Readable Artefacts

The following files in `.machine_readable/` contain structured project metadata:

- `STATE.scm` - Current project state and progress
- `META.scm` - Architecture decisions and development practices
- `ECOSYSTEM.scm` - Position in the ecosystem and related projects
- `AGENTIC.scm` - AI agent interaction patterns
- `NEUROSYM.scm` - Neurosymbolic integration config
- `PLAYBOOK.scm` - Operational runbook

## Pending Integration: panic-attack → hypatia → gitbot-fleet (Sonnet Task)

### Data Flow (NOT YET IMPLEMENTED)

```
panic-attack assail (scan repos)
        ↓ JSON results
verisimdb-data repo (git-backed flat-file store)
        ↓ read scan results
hypatia Logtalk rules (pattern detection)
        ↓ actionable findings
gitbot-fleet dispatcher
        ├── sustainabot: EcoScore/EconScore from scan metrics
        ├── echidnabot: proof obligations ("verify fix resolves weak points")
        └── rhodibot: automated fix suggestions
```

### What Needs Building

1. **verisimdb connector**: Logtalk rules that read scan results from verisimdb-data repo
   - Clone/pull verisimdb-data, parse JSON scan results
   - Transform weak points into Logtalk facts
   - Example fact: `weak_point(echidna, "src/prover.rs", unsafe_block, high)`

2. **Pattern detection rules**: Logtalk rules that fire on scan data
   - "3+ repos share same unsafe pattern" → fleet-wide advisory
   - "Repo X has critical weak points increasing over time" → sustainabot alert
   - "New weak point type detected" → echidnabot proof request

3. **Fleet dispatch**: Route findings to appropriate bots via GraphQL
   - Each bot has a GraphQL mutation for receiving findings
   - Hypatia decides which bot handles which finding type

### Hardware Integration (Future)

hardware-crash-team findings can also flow through hypatia:
- Zombie device patterns across fleet machines
- Driver conflict advisories
- Remediation success/failure tracking

## Language Policy (Hyperpolymath Standard)

### ALLOWED Languages & Tools

| Language/Tool | Use Case | Notes |
|---------------|----------|-------|
| **ReScript** | Primary application code | Compiles to JS, type-safe |
| **Deno** | Runtime & package management | Replaces Node/npm/bun |
| **Rust** | Performance-critical, systems, WASM | Preferred for CLI tools |
| **Tauri 2.0+** | Mobile apps (iOS/Android) | Rust backend + web UI |
| **Dioxus** | Mobile apps (native UI) | Pure Rust, React-like |
| **Gleam** | Backend services | Runs on BEAM or compiles to JS |
| **Bash/POSIX Shell** | Scripts, automation | Keep minimal |
| **JavaScript** | Only where ReScript cannot | MCP protocol glue, Deno APIs |
| **Nickel** | Configuration language | For complex configs |
| **Guile Scheme** | State/meta files | STATE.scm, META.scm, ECOSYSTEM.scm |
| **Julia** | Batch scripts, data processing | Per RSR |
| **OCaml** | AffineScript compiler | Language-specific |
| **Ada** | Safety-critical systems | Where required |

### BANNED - Do Not Use

| Banned | Replacement |
|--------|-------------|
| TypeScript | ReScript |
| Node.js | Deno |
| npm | Deno |
| Bun | Deno |
| pnpm/yarn | Deno |
| Go | Rust |
| Python | Julia/Rust/ReScript |
| Java/Kotlin | Rust/Tauri/Dioxus |
| Swift | Tauri/Dioxus |
| React Native | Tauri/Dioxus |
| Flutter/Dart | Tauri/Dioxus |

### Mobile Development

**No exceptions for Kotlin/Swift** - use Rust-first approach:

1. **Tauri 2.0+** - Web UI (ReScript) + Rust backend, MIT/Apache-2.0
2. **Dioxus** - Pure Rust native UI, MIT/Apache-2.0

Both are FOSS with independent governance (no Big Tech).

### Enforcement Rules

1. **No new TypeScript files** - Convert existing TS to ReScript
2. **No package.json for runtime deps** - Use deno.json imports
3. **No node_modules in production** - Deno caches deps automatically
4. **No Go code** - Use Rust instead
5. **No Python anywhere** - Use Julia for data/batch, Rust for systems, ReScript for apps
6. **No Kotlin/Swift for mobile** - Use Tauri 2.0+ or Dioxus

### Package Management

- **Primary**: Guix (guix.scm)
- **Fallback**: Nix (flake.nix)
- **JS deps**: Deno (deno.json imports)

### Security Requirements

- No MD5/SHA1 for security (use SHA256+)
- HTTPS only (no HTTP URLs)
- No hardcoded secrets
- SHA-pinned dependencies
- SPDX license headers on all files

