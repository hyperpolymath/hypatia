# czech-file-knife - Security Audit

## Repository Info
- **URL:** https://github.com/hyperpolymath/czech-file-knife
- **Language:** Rust (workspace)
- **Description:** Universal file management tool with cloud provider integration
- **Audit Date:** 2025-12-29

---

## Issues Summary
- **Total Open Alerts:** 7
- **Workflow/Deps:** 2
- **Vulnerabilities:** 1
- **Process/Scorecard:** 4

---

## Vulnerability Issues

### 1. VulnerabilitiesID - Dependency Vulnerabilities
- **Severity:** VARIES
- **Status:** OPEN

#### Detection
```bash
cargo audit
```

#### Common Issues in File Management Tools
- `fuser` crate - FUSE bindings may have platform-specific issues
- `reqwest` - HTTP client vulnerabilities
- `oauth2` - Authentication library updates

#### Fix Steps
1. Run `cargo audit` to identify specific CVEs
2. Update Cargo.toml with patched versions
3. Run `cargo update` to update lockfile
4. Verify with `cargo audit` again

---

## Workflow Issues

### 2. PinnedDependenciesID - Unpinned Actions (2 files)

| File | Issue |
|------|-------|
| `.clusterfuzzlite/Dockerfile` | Base images not SHA-pinned |
| `.github/workflows/comprehensive-quality.yml` | Actions not SHA-pinned |

#### Dockerfile Fix
```dockerfile
# BAD
FROM rust:1.83-slim

# GOOD - Use digest
FROM rust:1.83-slim@sha256:abc123... # Use actual digest
```

#### Workflow Fix
Use SHA-pinned action references as documented in CLAUDE.md.

---

## OpenSSF Scorecard Issues

### 3. SecurityPolicyID - Missing SECURITY.md
- **Status:** OPEN
- **Fix:** Add SECURITY.md

### 4. MaintainedID - Activity
- **Status:** OPEN
- **Fix:** Organic activity

### 5. CodeReviewID - No Required Reviews
- **Status:** OPEN
- **Fix:** Enable branch protection with required reviews

### 6. CIIBestPracticesID - No Badge
- **Status:** OPEN
- **Fix:** Register at bestpractices.coreinfrastructure.org

---

## Workspace Security Considerations

czech-file-knife is a Rust workspace with 8 crates:
- cfk-core
- cfk-providers
- cfk-cache
- cfk-search
- cfk-vfs
- cfk-cli
- cfk-integrations
- cfk-ios

### Per-Crate Audit
```bash
# Audit entire workspace
cargo audit

# Check specific crate
cargo audit -p cfk-providers
```

### High-Risk Components
| Crate | Risk Area | Mitigation |
|-------|-----------|------------|
| cfk-providers | OAuth tokens, cloud credentials | Never log credentials |
| cfk-vfs | FUSE mount, kernel interaction | Fuzz test VirtualPath |
| cfk-cache | Cached data encryption | Use authenticated encryption |

---

## ClusterFuzzLite Dockerfile Fix

```dockerfile
# SPDX-License-Identifier: PMPL-1.0-or-later

# Pin base image with SHA256 digest
FROM gcr.io/oss-fuzz-base/base-builder-rust@sha256:abc123...

# Install dependencies
RUN apt-get update && apt-get install -y \
    libfuse3-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Copy source
COPY . /src/czech-file-knife
WORKDIR /src/czech-file-knife

# Build fuzz targets
RUN cargo +nightly fuzz build
```

---

## Prevention Rules (Logtalk)

```logtalk
% Require cargo audit in Rust CI
rust_rule(missing_audit, preventive,
    "Rust repos must run cargo audit in CI",
    [
        file_exists("Cargo.toml"),
        not(workflow_contains("cargo audit"))
    ],
    block_release).

% Workspace-aware dependency check
rust_rule(workspace_vuln, curative,
    "Check all workspace members for vulnerabilities",
    [
        file_exists("Cargo.toml"),
        contains("[workspace]"),
        any_member_has_vuln
    ],
    alert_maintainer).
```

---

## Training Classification
- **Error Types:** DEPENDENCY_VULN, WORKFLOW_SECURITY, PROCESS_HYGIENE
- **Auto-Fixable:** PARTIAL (workflow yes, deps need review)
- **Prevention:** cargo audit in CI, Dependabot
- **Recurrence Risk:** MEDIUM (transitive deps common)
