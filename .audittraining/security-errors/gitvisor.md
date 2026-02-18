# gitvisor - Security Audit

## Repository Info
- **URL:** https://github.com/hyperpolymath/gitvisor
- **Language:** Elixir/Phoenix, ReScript, Ada
- **Description:** Git repository visualization and management platform
- **Audit Date:** 2025-12-29

---

## Issues Summary
- **Total Open Alerts:** 8
- **Process/Scorecard:** 8
- **Code Issues:** 0

---

## OpenSSF Scorecard Issues

### 1. SecurityPolicyID - Missing SECURITY.md
- **Severity:** MEDIUM
- **Status:** OPEN

#### Fix
Create `SECURITY.md` at repository root with vulnerability reporting instructions.

---

### 2. SASTID - No Static Analysis
- **Severity:** MEDIUM
- **Status:** OPEN

#### Problem
No SAST (Static Application Security Testing) configured. For a multi-language repo:
- Elixir: Use `sobelow` or `credo`
- ReScript: Use `rescript` compiler warnings
- Ada: Use `gnatcheck` or SPARK

#### Fix
Add CodeQL workflow with appropriate language matrix:
```yaml
# SPDX-License-Identifier: PMPL-1.0-or-later
name: CodeQL

permissions: read-all

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]
  schedule:
    - cron: '0 6 * * 1'

jobs:
  analyze:
    runs-on: ubuntu-latest
    permissions:
      security-events: write
    strategy:
      matrix:
        language: ['actions']  # Add others if CodeQL supports them
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4
      - uses: github/codeql-action/init@662472033e021d55d94146f66f6058822b0b39fd # v3
        with:
          languages: ${{ matrix.language }}
      - uses: github/codeql-action/autobuild@662472033e021d55d94146f66f6058822b0b39fd # v3
      - uses: github/codeql-action/analyze@662472033e021d55d94146f66f6058822b0b39fd # v3
```

---

### 3. CITestsID - No CI Tests
- **Severity:** MEDIUM
- **Status:** OPEN

#### Problem
No automated test workflow detected.

#### Fix
Add test workflow:
```yaml
# SPDX-License-Identifier: PMPL-1.0-or-later
name: Tests

permissions: read-all

on:
  push:
    branches: [main]
  pull_request:

jobs:
  elixir-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4
      - uses: erlef/setup-beam@5304e04ea2b355f03681464e683d92e3b2f18451 # v1
        with:
          elixir-version: '1.16'
          otp-version: '26'
      - run: mix deps.get
      - run: mix test

  rescript-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4
      - uses: denoland/setup-deno@e59eec6ea09fa8bf93aed2e83c5ca18b6f840257 # v2
      - run: deno task build
      - run: deno task test

  ada-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4
      - uses: alire-project/setup-alire@69aa99f7d21a8df8a016d7256ae95829b04e5e63 # v3
      - run: cd tui && alire build
      - run: cd tui && alire run -- --test
```

---

### 4. MaintainedID - Activity
- **Status:** OPEN
- **Fix:** Organic commit activity (no automation)

### 5. FuzzingID - No Fuzzing
- **Status:** OPEN
- **Fix:** Add ClusterFuzzLite for Elixir/Rust components

### 6. CodeReviewID - No Required Reviews
- **Status:** OPEN
- **Fix:** Enable branch protection with required reviews

### 7. CIIBestPracticesID - No Badge
- **Status:** OPEN
- **Fix:** Register at bestpractices.coreinfrastructure.org

### 8. BranchProtectionID - Not Enabled
- **Status:** OPEN
- **Fix:** Enable branch protection via API

---

## Multi-Language SAST Strategy

For polyglot repos like gitvisor:

| Component | Language | SAST Tool | Integration |
|-----------|----------|-----------|-------------|
| Backend | Elixir | sobelow | `mix sobelow` in CI |
| Frontend | ReScript | Built-in | Compiler warnings |
| TUI | Ada | gnatcheck | `gnatcheck` in CI |
| Infra | Actions | CodeQL | GitHub native |

---

## Prevention Rules (Logtalk)

```logtalk
% Require CI tests for all repos
project_rule(missing_ci_tests, preventive,
    "Repos must have automated test workflow",
    [
        not(file_exists(".github/workflows/*test*.yml")),
        has_source_code
    ],
    block_release).

% Require SAST for multi-language repos
project_rule(missing_sast, preventive,
    "Multi-language repos must have SAST configured",
    [
        language_count > 1,
        not(file_exists(".github/workflows/codeql.yml"))
    ],
    warn_maintainer).
```

---

## Training Classification
- **Error Types:** PROCESS_HYGIENE, MISSING_TESTS, MISSING_SAST
- **Auto-Fixable:** YES (add workflow files)
- **Prevention:** Template enforcement, repo standards
- **Recurrence Risk:** MEDIUM (new repos may lack setup)
