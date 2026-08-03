<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# PARKED DRAFT: Central Estate CI/CD Audit workflow

**Status: not landable.** This was found untracked in `.github/workflows/main-estate-audit.yml`
(2026-08-03) and parked here rather than committed, because as written it fails on the first
step of every run and violates repo doctrine on several axes:

1. **Every gate action is unresolvable** — `hyperpolymath/cicd-suite` returns 404 (repo does
   not exist or is not visible). All 26 `uses:` steps reference it.
2. **Nothing is SHA-pinned** — 26× `@main` plus `actions/checkout@v4` (repo is on v7.0.1,
   SHA-pinned, everywhere else). Violates the Security Requirements in `.claude/CLAUDE.md`
   and hypatia's own supply-chain rules; the self-scan would flag this file.
3. `hyperpolymath/cicd-suite` is **not in the Actions allow-list**
   (`scripts/ci-health/action-superset.txt`) — the exact B-ALLOWLIST/ERR-SEC-003 failure class.
4. **No `permissions:` block** (needs read-only), **no `concurrency:`** (trips WF021 on a
   push+PR checker), **no `timeout-minutes:`** (26 sequential network-bound gates, no ceiling),
   **no SPDX header**, and the file mode was 0755 where workflows are 0644.
5. Despite the name it is not an estate-wide cron — triggers are only this repo's own
   `push`/`pull_request` on `main`. 26 sequential gates in one job means the first failure
   hides the other 25; a matrix or `continue-on-error` + summary step is needed for
   diagnosability.

**To revive:** create (or locate) the `cicd-suite` composite actions, SHA-pin every reference,
add them to the allow-list, add `permissions`/`concurrency`/`timeout-minutes`/SPDX, and decide
the real trigger model (schedule + workflow_dispatch if it is genuinely estate-scoped).

## Original draft (verbatim)

```yaml
name: Central Estate CI/CD Audit

on:
  push:
    branches: [ "main" ]
  pull_request:
    branches: [ "main" ]

jobs:
  estate-audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Required Files Gate
        uses: hyperpolymath/cicd-suite/actions/required-files-check@main

      - name: Code Hygiene Gate
        uses: hyperpolymath/cicd-suite/actions/code-hygiene-check@main

      - name: Manifest Validation Gate
        uses: hyperpolymath/cicd-suite/actions/manifest-check@main

      - name: Idris2 ABI Purity Gate
        uses: hyperpolymath/cicd-suite/actions/idris2-abi-check@main

      - name: Zig Hexadeca API Gate
        uses: hyperpolymath/cicd-suite/actions/zig-hexadeca-check@main

      - name: Contractile Validation Gate
        uses: hyperpolymath/cicd-suite/actions/contractile-validation-check@main

      - name: Recipes Set Validation Gate
        uses: hyperpolymath/cicd-suite/actions/recipes-set-check@main

      - name: Affirmation Document Gate
        uses: hyperpolymath/cicd-suite/actions/affirmation-check@main

      - name: Academic Referencing Gate
        uses: hyperpolymath/cicd-suite/actions/referencing-check@main

      - name: Semantic Audit Gate
        uses: hyperpolymath/cicd-suite/actions/semantic-audit-check@main

      - name: SPDX License Gate
        uses: hyperpolymath/cicd-suite/actions/spdx-license-check@main

      - name: Proof Runner Gate
        uses: hyperpolymath/cicd-suite/actions/proof-runner-check@main

      - name: PRAT Testing Gate
        uses: hyperpolymath/cicd-suite/actions/prat-check@main

      - name: Panic Attack & Pons Gate
        uses: hyperpolymath/cicd-suite/actions/custom-tools-check@main

      - name: WWW & Well-Known Compliance Gate
        uses: hyperpolymath/cicd-suite/actions/www-compliance-check@main

      - name: BoJ Cartridge Validation Gate
        uses: hyperpolymath/cicd-suite/actions/boj-cartridge-check@main

      - name: Formatting Validation Gate
        uses: hyperpolymath/cicd-suite/actions/formatting-check@main

      - name: Accreditations & Badges Gate
        uses: hyperpolymath/cicd-suite/actions/badges-check@main

      - name: Metrics Extraction Gate
        uses: hyperpolymath/cicd-suite/actions/metrics-check@main

      - name: Linguist & Banned Languages Gate
        uses: hyperpolymath/cicd-suite/actions/linguist-check@main

      - name: Test & Benchmarks Dashboard Gate
        uses: hyperpolymath/cicd-suite/actions/tests-benches-check@main

      - name: Hosting & Site Status Gate
        uses: hyperpolymath/cicd-suite/actions/hosting-check@main

      - name: Git-Sea Analytics Gate
        uses: hyperpolymath/cicd-suite/actions/gitsea-check@main

      - name: Trust & Humans Validation Gate
        uses: hyperpolymath/cicd-suite/actions/trust-humans-check@main

      - name: Are We UnAPI Gate (Secret Scanning)
        uses: hyperpolymath/cicd-suite/actions/secrets-check@main

      - name: Reasonably Good Token Validation Gate
        uses: hyperpolymath/cicd-suite/actions/vaulted-tokens-check@main
```
