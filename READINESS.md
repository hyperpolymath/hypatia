<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# hypatia Component Readiness Assessment

**Standard:** [Component Readiness Grades (CRG) v1.0](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)
**Assessed:** 2026-04-04
**Assessor:** Jonathan D.A. Jewell + Claude Sonnet 4.6

**Current Grade:** B

## Summary

| Component             | Grade | Release Stage      | Evidence Summary                                                                 |
|-----------------------|-------|--------------------|----------------------------------------------------------------------------------|
| `pattern-analyzer`    | B     | Release Candidate  | 15 rule modules, 609 tests; scans 283+ repos estate-wide across all languages via hypatia-scan.yml CI workflow. |
| `recipe-matcher`      | B     | Release Candidate  | 56 recipes validated; wired in CI on Rust, Elixir, Gleam, Julia, ReScript, Idris2, Zig, OCaml, Ada, Haskell targets. |
| `triangle-router`     | C     | Beta               | Routes findings through neurosymbolic pipeline; validated on hypatia itself and 10+ repos. |
| `fleet-dispatcher`    | C     | Beta               | Dispatches gitbot-fleet directives (rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot); tested on 6+ repos but dispatch logs incomplete. |
| `neural-coordinator`  | C     | Beta               | Blackboard architecture for multi-agent coordination; dogfooded on hypatia CI for all repos but external API callers limited. |
| `hypatia-scan.yml`    | B     | Release Candidate  | Deployed as required CI workflow on all RSR repos (283+); diverse language targets confirmed. |

## Overall Project Readiness

- **Components at B or above:** 3/6 (50%) — pattern-analyzer, recipe-matcher, hypatia-scan.yml
- **Components at C (Beta) or above:** 6/6 (100%)
- **Components at D (Alpha):** 0/6 (0%)
- **Weighted assessment:** The primary use case (neurosymbolic static analysis + CI enforcement across the estate) is **Grade B**. Neural coordination and fleet dispatch are Beta-quality.

## Detailed Assessment

### `pattern-analyzer` — Neurosymbolic Pattern Analysis Engine (Grade: B)

**Evidence:**
- 15 rule modules covering security, quality, style, and correctness patterns
- 609 tests passing across all rule modules
- Deployed via `hypatia-scan.yml` on all RSR-compliant repos (283+)
- Language diversity confirmed across external scan targets:
  1. Elixir/OTP (burble, oblibeny) — Phoenix, GenServer, Ecto patterns
  2. Rust systems code (panic-attacker, januskey, conflow, a2ml-rs) — unsafe, FFI, unwrap
  3. Gleam/BEAM (k9_gleam, a2ml_gleam, polyglot-formalisms-gleam) — typed BEAM target
  4. Idris2/formal-verified (ephapax, stapeln) — dependent type code
  5. Julia scientific (7-tentacles, statistease, developer-ecosystem) — REPL scripting
  6. ReScript/Deno (idaptik, nafa-app) — web frontend code
  7. Ada/SPARK (safety-critical components) — safety-critical language
  8. OCaml (affinescript compiler) — functional language
  9. Haskell (a2ml-haskell) — pure functional
  10. Zig (boj-server FFI layers) — systems language
- Findings fed back to estate improvement loop (19K TODOs detected estate-wide in 2026-03-22 codebase audit)

**Known limitations:**
- Some framework detection fires on wrong-language codebases (cross-language false positives)
- Blackboard migration to topology v2 in progress — some rules bypass the blackboard
- Idris2 proofs not yet verified by echidnabot in CI (requires idris2 in CI image)

**Promotion path to A:** External users outside hyperpolymath confirm value; no harmful findings in wild.

### `recipe-matcher` — Recipe Execution and Validation (Grade: B)

**Evidence:**
- 56 Justfile recipes defined and exercised
- Matched against targets in all primary hyperpolymath languages
- Wired into `hypatia-scan.yml` enforcement on 283+ repos
- Validated on diverse build systems: cargo, deno, mix, gleam, zig build, julia, just

**Known limitations:**
- Recipe coverage for Idris2/Pack build system is partial
- Custom monorepo Justfiles sometimes need manual override

**Promotion path to A:** External users validate recipes in non-hyperpolymath contexts.

### `triangle-router` — Neurosymbolic Pipeline Router (Grade: C)

**Evidence:**
- Routes findings through symbolic → neural → symbolic triangle
- Validated on hypatia itself and 10+ estate repos
- Integrates with blackboard for cross-agent sharing

**Known limitations:**
- Not validated on more than 15 external targets
- Neural component requires specific model availability

**Promotion path to B:** Validated on 6+ diverse external repos with confirmed finding routing.

### `fleet-dispatcher` — Gitbot Fleet Dispatch (Grade: C)

**Evidence:**
- Dispatches directives to all 6 gitbot-fleet bots
- Tested on hypatia, panic-attacker, januskey, stapeln, ephapax, boj-server
- Directive schemas validated against bot contracts

**Known limitations:**
- Dispatch confirmation logs incomplete; some directives may be lost silently
- Bot response correlation not fully implemented

**Promotion path to B:** Confirmed round-trip dispatch+response on 6+ diverse targets.

### `neural-coordinator` — Multi-Agent Blackboard Coordinator (Grade: C)

**Evidence:**
- Blackboard architecture provides shared working memory for all agents
- Coordinates hypatia-scan findings with gitbot-fleet and echidnabot
- Dogfooded on all CI runs across the estate

**Known limitations:**
- External API callers (non-gitbot clients) are limited in number
- Blackboard topology migration in progress (v2 not fully deployed)

**Promotion path to B:** Wired to 6+ diverse external API consumers.

### `hypatia-scan.yml` — GitHub Actions CI Workflow (Grade: B)

**Evidence:**
- Required CI workflow deployed on all RSR-compliant repos (283+ confirmed)
- Diverse target types: Rust, Elixir, Gleam, Julia, ReScript, Idris2, Zig, OCaml, Ada, Haskell, 007-lang
- SHA-pinned actions, `permissions: read-all`, SPDX headers present
- Enforcement: blocks merge on critical findings (configurable threshold)

**Known limitations:**
- Some repos have outdated SHA pins requiring periodic refresh
- hypatia binary not always in PATH in CI images (fallback to cargo run)

**Promotion path to A:** External maintainers outside hyperpolymath adopt and report no false-positive blocking.

## Test Category Matrix

| # | Category    | Status  | Count | Recipe             | Notes                                      |
|---|-------------|---------|-------|--------------------|--------------------------------------------|
| 1 | Unit        | ✓       | 609   | `just test`        | All 15 rule modules covered                |
| 2 | Integration | PARTIAL | CI    | `just test-ci`     | hypatia-scan.yml runs on push              |
| 3 | E2E         | PARTIAL | ~10   | `just test-e2e`    | Scan outputs validated on real repos       |
| 4 | Property    | MISSING | 0     | —                  | No proptest/quickcheck yet                 |
| 5 | Fuzz        | MISSING | 0     | —                  | Pattern input fuzzing not configured       |
| 6 | Mutation    | MISSING | 0     | —                  | cargo-mutants not yet applied to hypatia   |

## Recipes

```
just test           # All 609 rule module tests
just test-ci        # CI integration tests
just test-e2e       # End-to-end scan against real repos
just build          # cargo build --release
just scan-self      # Scan hypatia with hypatia (dogfood)
just lint           # Clippy + fmt checks
```

## Known Debt

- Blackboard topology v2 migration incomplete
- Idris2 proof checking not wired in CI
- No mutation testing on rule modules
- Fleet dispatch logs incomplete
- Some SHA pins in hypatia-scan.yml need periodic refresh
