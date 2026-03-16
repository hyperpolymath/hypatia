<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Generated: 2026-03-15 -->
# Hypatia Development — What We Want It To Actually Do

## Current State

Hypatia is a neurosymbolic CI/CD engine that scans repos. It has:
- `hypatia-scan.yml` workflow in every RSR repo
- `.hypatia/` directory for scan state (activity.jsonl, last-visit.json)
- Integration with VeriSimDB for storing scan results
- Integration with gitbot-fleet for dispatching fixes

## What We Want It To Do (decided 2026-03-15)

### 1. Root Hygiene Enforcement

**Built today:** `root-hygiene.scm` + `root-hygiene-scan.json`

- Scan repo root against allowed/banned file lists
- Flag stale snapshots, executed plans, duplicate docs, wrong file names
- Dispatch rhodibot (auto-fix), finishbot (PR for moves/deletes), seambot (verify)
- Watch rsr-template-repo for structural drift
- Repos can exclude rules via their own `bot_directives/` overrides

### 2. OpenSSF Badge Assessment

**Designed today, needs building:**

- Auto-detect Passing/Silver/Gold criteria per repo
- File presence checks (LICENSE, SECURITY.md, CONTRIBUTING.md, etc.)
- GitHub API checks (branch protection, 2FA, PR review stats)
- Workflow analysis (CodeQL, fuzzing, coverage, signing present?)
- Git history analysis (bus factor via `git shortlog -s`)
- Content analysis (SPDX headers, DCO mention, crypto usage)
- Generate badge assessment report per repo
- Submit to bestpractices.dev API for qualifying repos
- Auto-embed badges in README
- Re-scan periodically, alert if repo falls below current level

### 3. SCM → A2ML Migration Scanner

**Rules added today:**

- Flag `.scm` state files for migration to `.a2ml`
- Flag `AI.djot` for deletion (superseded)
- Advisory severity — flag but don't auto-convert (needs review)
- Automated conversion is feasible (both structured formats)

### 4. Security Audit

**Needs building:**

- Dependabot alert aggregation across all repos
- Secret scanning verification (enabled on all repos?)
- CodeQL coverage check (running on repos with supported languages?)
- Leaked secrets detection in non-code files (configs, memory files, docs)
- Supply chain integrity: SLSA provenance, Sigstore signing, SBOM generation
- Vulnerability summary dashboard (which repos have open alerts?)

### 5. CI/CD Audit

**Needs building:**

- Verify all 17 standard workflows present per repo
- Check workflow SHA pins against current known-good values
- Detect duplicate/conflicting workflows
- Verify mirror.yml + instant-sync.yml functioning for GitLab/Bitbucket
- Detect workflows with `permissions: write-all` (should be `read-all` + specific)

### 6. RSR Compliance Audit

**Partially done (25/149 repos audited manually 2026-03-02):**

- Template placeholder replacement (hypatia, hyperpolymath, github.com)
- SPDX header correctness (AGPL → PMPL migration)
- Author/email correctness
- .machine_readable/ directory structure
- .well-known/ presence
- CODEOWNERS presence
- Contractile files correctness (PLMP/PMLP typos)

### 7. Green Web + Website Standards

**For launched websites:**

- Green Web Foundation certification check
- Mozilla Observatory security headers scan (aim for A+)
- WCAG 2.1 AA accessibility check (axe-core automated)
- Carbon footprint per page (websitecarbon.com API)
- Auto-badge websites that pass

### 8. Honest Completion Audit

**Critical — addresses the "inflated percentages" weakness:**

- Parse STATE.scm/STATE.a2ml from every repo
- Cross-reference claimed completion against actual file counts, test counts, build status
- Flag repos where claimed completion > detectable completion
- Generate honest completion report
- This is the antidote to Gemini damage and optimistic self-assessment

## Architecture

```
Hypatia (scan engine)
    ├── On-visit triggers (per-repo)
    │   ├── root-hygiene scan
    │   ├── badge assessment
    │   ├── security audit
    │   ├── CI/CD audit
    │   ├── RSR compliance
    │   └── honest completion check
    │
    ├── Cross-repo aggregation
    │   ├── Security dashboard
    │   ├── Badge status board
    │   ├── Completion heat map
    │   └── Template drift report
    │
    ├── Fleet dispatch
    │   ├── rhodibot → auto-fix formatting/metadata
    │   ├── finishbot → PRs for structural changes
    │   ├── seambot → integration verification
    │   ├── echidnabot → proof verification
    │   ├── sustainabot → dependency updates
    │   └── panicbot → security scanning
    │
    └── External integrations
        ├── bestpractices.dev API (badge submission)
        ├── Green Web Foundation API
        ├── Mozilla Observatory API
        ├── VeriSimDB (scan result storage)
        └── PanLL dashboard (visualisation)
```

## Implementation Priority

1. **Badge assessment** — highest visibility, proves quality
2. **Honest completion audit** — addresses the biggest weakness
3. **Security audit aggregation** — essential for credibility
4. **Root hygiene enforcement** — already built, just needs fleet rollout
5. **CI/CD audit** — important but less urgent
6. **RSR compliance sweep** — large but mechanical
7. **Green Web / website standards** — nice to have, do when sites launch
