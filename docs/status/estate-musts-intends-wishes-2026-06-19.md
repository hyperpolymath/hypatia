<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) -->

# Estate status — musts / intends / wishes

**Snapshot:** 2026-06-19 (UTC) · authored by Claude (Opus 4.8)

Framing: **must** = normative/blocking (hypatia's own rules are the estate norm) · **intend** = planned/in-flight · **wish** = aspirational. `READINESS.md` formally grades 6 bots (assessed 2026-04-04); the other 5 rows are from issues + the 2026-06 session and marked _(inferred)_.

## Where we are

Core three bots + coordinator are Grade B/Beta and operating estate-wide (1635 dispatches, 99% success). The 2026-06 session cleared the estate's Nix→Guix, CI-timeout, workflow_audit-FP and hexadeca-contract musts. The big live workstream is the merge-orchestration runtime (parallel session, now merged in hypatia). Remaining musts are mostly blocked on the owner.

## hypatia — the normative anchor

| MUST | INTEND | WISH |
|---|---|---|
| self-scan must run current rules — deployed scanner lags `main` (#484) · close `record_outcome` loop · neural-convergence proofs (#486, BLOCKED: Mathlib net) | deploy verisim-api · code-scanning backlog triage (#470/#369) · M13 SARIF / M14 GraphQL / M15 auth | Nx/EXLA backend · cross-org VCL federation · flesh out bebop/capnproto bodies on demand |
| ✅ this session: hexadeca contract drift-guarded (#510) + documented (#511); workflow_audit FPs fixed (#462); Guix-only; CI timeouts | | |

## gitbot-fleet — whole

| MUST | INTEND | WISH |
|---|---|---|
| triage 6 Dependabot alerts 1H/1M/4L (#278) · close `record_outcome` loop · dispatch PAT ✅ | consume Hypatia route/blast-radius metadata (#264) · LicensePolicy never-auto-execute (#253) · bots gain signed `attest` verbs | 29 `expect_in_hot_path` hygiene (#255) · SafeDOM standing recipe (#214) · promote C-grade bots to B |
| ✅ this session: Nix removed (#260/#262); findings-submissions documented; Bustfile (#261) | | |

## each gitbot

| Bot | Grade | MUST | INTEND | WISH |
|---|---|---|---|---|
| rhodibot (git ops) | B | keep 9/9 dispatch flows green | signed `attest` verb | — |
| echidnabot (quality/proofs) | B | — | consume typed `VerifyOutcome` (#245) | SDK packaging surface |
| sustainabot (eco/econ) | B | fix broken Cargo path dep on missing `bots/panic-attacker` (#246) | SafeDOM recipe (#214) · add `guix.scm` | — |
| glambot (aesthetic) | C | promote C→B (calibration) | apply suggestions systematically | richer visual analyzers |
| seambot (seams) | C | promote C→B | cross-repo seam data | — |
| finishingbot (completion) | C | promote C→B (calibration) | — | — |
| gsbot (Discord, Rust/SPARK) _(inf)_ | — | no Python ✅; Bronze RSR | add `guix.scm` | SPARK-verify `domain.rs` |
| cipherbot _(inf)_ | — | clear secret-FP in `analyzers/infra.rs` + `expect` hygiene (#207/#255) | — | — |
| accessibilitybot _(inf)_ | — | `expect_in_hot_path` hygiene (#255) | — | — |
| panicbot _(inf)_ | — | reach-classification correctness | consume patch-bridge registry (#358) | — |
| the-hotchocolabot _(inf)_ | — | it's a stub — needs real impl or status clarity | build out | — |

## .git-private-farm — itself

| MUST | INTEND | WISH |
|---|---|---|
| clear the GitHub Actions billing wall (blocks actuator) · receiver workflow for sha-bump propagation (#67) | pool-driven privileged merge actuator (P0–P3 + mass-squash) · guarded Hypatia fanout: canary/dedupe/kill-switch (#74) | auto-remediate mirror drift (#75) |
| ✅ this session: Nix removed (#71/#73); README dedup + k9 self-validating + Dust/Bust (#72) | | |

## Common across the estate

| Scope | MUST | INTEND | WISH |
|---|---|---|---|
| All 3 | Guix-only ✅ · SPDX headers · resolve held 6a2 STATE/META/ANCHOR + root-vs-`6a2/` layout (UNRESOLVED) | merge-orchestration runtime tier (hypatia=decision · fleet=attest · farm=actuator) + `.machine_readable` runtime tier · bot_directives redesign | wikis → bleeding-edge (#482) · README/EXPLAINME + contractile currency (#483) |
| ≥2 | `record_outcome` loop (hypatia+fleet) · dependency/CVE triage (fleet #278, hypatia #330) | doc-currency pass (#483) | rot/unused sweep (#483) |

## Blocked on the owner

- Deploy verisim-api (unblocks native modalities + connector bodies).
- Widen Mathlib allowlist or run local → proofs #486.
- Clear .git-private-farm Actions billing → farm actuator.
- Add `standards` + `rsr-template` to a session's scope → divergence audit #485.
- Delete 2 stale hypatia branches (`test/ci-codeql-hypatia`, `feat/sha-bump-propagation-rule-418`) → clears GS007.

## Tracked issues

hypatia #482 (wikis), #483 (doc/contractile/rot currency), #484 (scanner re-release), #485 (divergence, needs-owner), #486 (proofs, needs-network) · gitbot-fleet #278 (Dependabot triage).
