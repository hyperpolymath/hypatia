# ubicity - Security Audit

## Repository Info
- **URL:** https://github.com/hyperpolymath/ubicity
- **Language:** JavaScript/Deno, ReScript, Rust (WASM)
- **Description:** Learning capture system for urban informal education
- **Audit Date:** 2025-12-29

---

## Issues Summary
- **Total Open Alerts:** 10
- **Workflow Security:** 4
- **Process/Scorecard:** 6

---

## Workflow Security Issues

### 1. Missing Workflow Permissions
- **Rule ID:** `TokenPermissionsID`, `actions/missing-workflow-permissions`
- **Severity:** MEDIUM
- **File:** `.github/workflows/rsr-antipattern.yml`
- **Status:** OPEN

#### Problem
Workflow lacks explicit `permissions:` declaration, inheriting default (often write) permissions.

#### Fix
Add at workflow level:
```yaml
# SPDX-License-Identifier: PMPL-1.0-or-later
name: RSR Antipattern Check

permissions: read-all

on:
  push:
  pull_request:
# ...
```

---

### 2. Unpinned Dependencies (3 files)

| File | Issue |
|------|-------|
| `wasm/.clusterfuzzlite/Dockerfile` | Base images not SHA-pinned |
| `.github/workflows/rescript-deno-ci.yml` | Actions using `@v4` not SHA |
| `.github/workflows/rsr-antipattern.yml` | Actions using `@v4` not SHA |

#### Fix Pattern
```yaml
# BAD
- uses: actions/checkout@v4

# GOOD
- uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4
```

---

## OpenSSF Scorecard Issues

| Check | Status | Fix |
|-------|--------|-----|
| SecurityPolicyID | OPEN | Add SECURITY.md |
| MaintainedID | OPEN | Organic commit activity |
| CodeReviewID | OPEN | Enable required PR reviews |
| CIIBestPracticesID | OPEN | Register at bestpractices.coreinfrastructure.org |
| BranchProtectionID | OPEN | Enable branch protection |

---

## Batch Fix Script

```julia
# fix-ubicity-workflows.jl
using GitHub

const REPO = "hyperpolymath/ubicity"
const SHA_PINS = Dict(
    "actions/checkout@v4" => "actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4",
    "denoland/setup-deno@v2" => "denoland/setup-deno@e59eec6ea09fa8bf93aed2e83c5ca18b6f840257 # v2",
    "actions/cache@v4" => "actions/cache@6849a6489940f00c2f30c0fb92c6274307ccb58a # v4"
)

function fix_workflow(path::String)
    content = read(path, String)

    # Add permissions if missing
    if !contains(content, "permissions:")
        content = replace(content,
            r"^(name:.*\n)" => s"\1\npermissions: read-all\n")
    end

    # Pin actions
    for (old, new) in SHA_PINS
        content = replace(content, old => new)
    end

    write(path, content)
end
```

---

## Prevention Rules (Logtalk)

```logtalk
% Enforce permissions declaration
workflow_rule(missing_permissions, preventive,
    "Reject workflows without permissions declaration",
    [
        file_type(".github/workflows/*.yml"),
        not(contains("permissions:"))
    ],
    block_merge).

% Enforce SHA pinning
workflow_rule(unpinned_action, preventive,
    "Reject workflows with unpinned actions",
    [
        pattern("uses:.*@v[0-9]"),
        not(pattern("#.*v[0-9]"))
    ],
    block_merge).
```

---

## Security.md Template

```markdown
# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.3.x   | :white_check_mark: |
| < 0.3   | :x:                |

## Reporting a Vulnerability

Please report security vulnerabilities to security@hyperpolymath.org.

Do NOT open public issues for security vulnerabilities.

We will respond within 48 hours and provide a timeline for fixes.
```

---

## Training Classification
- **Error Types:** WORKFLOW_SECURITY, PROCESS_HYGIENE
- **Auto-Fixable:** YES (all workflow issues)
- **Prevention:** Workflow linter in CI
- **Recurrence Risk:** LOW with proper CI checks
