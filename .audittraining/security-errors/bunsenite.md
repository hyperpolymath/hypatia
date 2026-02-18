# bunsenite - Security Audit

## Repository Info
- **URL:** https://github.com/hyperpolymath/bunsenite
- **Language:** Rust
- **Description:** Nickel configuration file parser with multi-language FFI bindings
- **Audit Date:** 2025-12-29

---

## Issues Summary
- **Total Open Alerts:** 6
- **Vulnerabilities:** 1
- **Workflow/Deps:** 1
- **Process/Scorecard:** 4

---

## Vulnerability Issues

### 1. VulnerabilitiesID - Dependency Vulnerabilities
- **Severity:** MEDIUM
- **Status:** OPEN (wee_alloc was fixed)

#### Known Fixed Issues
- `wee_alloc` - Unmaintained → Fixed

#### Check for New Issues
```bash
cargo audit
```

#### Common Config Parser Vulnerabilities
| Risk | Description | Mitigation |
|------|-------------|------------|
| ReDoS | Regex denial of service | Use non-backtracking regex |
| Stack overflow | Deeply nested configs | Limit recursion depth |
| Memory exhaustion | Large config files | Set size limits |

---

## Workflow Issues

### 2. PinnedDependenciesID - ClusterFuzzLite
- **File:** `.clusterfuzzlite/Dockerfile`
- **Status:** OPEN

#### Fix
Pin base image with digest:
```dockerfile
# SPDX-License-Identifier: PMPL-1.0-or-later
FROM gcr.io/oss-fuzz-base/base-builder-rust@sha256:... # Pin digest
```

---

## OpenSSF Scorecard Issues

### 3. MaintainedID - Activity
- **Status:** OPEN
- **Note:** Has releases (1.0.2) - organic activity needed

### 4. CodeReviewID - No Required Reviews
- **Status:** OPEN
- **Fix:** Enable branch protection with 1+ reviews

### 5. CIIBestPracticesID - No Badge
- **Status:** OPEN
- **Fix:** Register at bestpractices.coreinfrastructure.org

### 6. BranchProtectionID - Not Enabled
- **Status:** OPEN
- **Fix:** Enable via API

---

## Config Parser Security Considerations

bunsenite parses Nickel configs. Security concerns:

### 1. Input Validation
```rust
// Limit config size
const MAX_CONFIG_SIZE: usize = 10 * 1024 * 1024; // 10MB

pub fn parse_config(input: &str) -> Result<Config, Error> {
    if input.len() > MAX_CONFIG_SIZE {
        return Err(Error::ConfigTooLarge);
    }
    // ...
}
```

### 2. Recursion Limits
```rust
const MAX_NESTING_DEPTH: usize = 100;

fn parse_value(input: &str, depth: usize) -> Result<Value, Error> {
    if depth > MAX_NESTING_DEPTH {
        return Err(Error::NestingTooDeep);
    }
    // ...
}
```

### 3. WASM Security
For the WASM bindings:
- Sandbox file system access
- Limit memory allocation
- No network access from config evaluation

---

## Fuzzing Targets

bunsenite should fuzz:
1. Config parsing (malformed Nickel)
2. FFI boundaries (invalid inputs from other languages)
3. WASM bindings (memory safety)

```rust
// fuzz/fuzz_targets/fuzz_parse.rs
#![no_main]
use libfuzzer_sys::fuzz_target;
use bunsenite::parse;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let _ = parse(input);
    }
});
```

---

## Prevention Rules (Logtalk)

```logtalk
% Config parsers need input limits
parser_rule(input_limits, preventive,
    "Config parsers must have input size limits",
    [
        crate_type(parser),
        not(pattern("MAX.*SIZE|size.*limit"))
    ],
    warn_reviewer).

% WASM crates need memory limits
wasm_rule(memory_limits, preventive,
    "WASM crates must limit memory allocation",
    [
        feature("wasm"),
        not(pattern("memory.*limit|max.*alloc"))
    ],
    warn_reviewer).
```

---

## Training Classification
- **Error Types:** DEPENDENCY_VULN, WORKFLOW_SECURITY, PROCESS_HYGIENE
- **Auto-Fixable:** YES (workflow and process fixes)
- **Prevention:** Fuzzing, input validation
- **Recurrence Risk:** LOW (mature crate)
