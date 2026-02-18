# Hypatia a2ml & K9 Integration

**Status:** ✅ IMPLEMENTED
**Date:** 2026-02-06
**Version:** 1.0.0

## Overview

Hypatia now integrates with:
- **a2ml** (AI Assistant Markup Language) - AI governance and workflow documentation
- **K9 Contractiles** (Nickel configuration) - Fleet-wide deployment and configuration management

This integration provides:
1. **Centralized Configuration:** Single source of truth for fleet settings
2. **AI Assistant Integration:** Standardized workflow for AI-driven rule management
3. **Type-Safe Config:** Nickel validates configuration at parse time
4. **Deployment Automation:** K9 contractiles enable automated fleet deployment

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Configuration Layer                                         │
├─────────────────────────────────────────────────────────────┤
│  rules.a2ml          AI assistant workflow instructions     │
│  fleet-config.k9.ncl Type-safe fleet configuration          │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│ Runtime Layer                                               │
├─────────────────────────────────────────────────────────────┤
│  read-k9-config.sh   Export K9 config to env vars          │
│  hypatia-cli.sh      Detection engine (reads env vars)     │
└────────────┬────────────────────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────────────────────┐
│ Integration Layer                                           │
├─────────────────────────────────────────────────────────────┤
│  fleet-coordinator   Orchestrates fleet-wide scans          │
│  ECHIDNA             Validates rules semantically           │
│  robot-repo-automaton Executes auto-fixes                   │
└─────────────────────────────────────────────────────────────┘
```

---

## File Structure

```
hypatia/
├── rules.a2ml                 # AI workflow documentation
├── fleet-config.k9.ncl        # K9 fleet configuration
├── read-k9-config.sh          # K9 → env var exporter
├── hypatia-cli.sh             # Scanner (reads env vars)
├── approved-rules/            # Production Logtalk rules
│   ├── technical_debt.lgt
│   ├── unsafe_without_doc.lgt
│   ├── eval_usage.lgt
│   └── DEPLOYMENT-MANIFEST-*.md
└── A2ML-K9-INTEGRATION.md     # This file
```

---

## Usage

### 1. Reading K9 Configuration

```bash
# Load K9 config into environment
source <(./read-k9-config.sh fleet-config.k9.ncl)

# Run scanner with K9 config
./hypatia-cli.sh scan /path/to/repo
```

### 2. Accessing Configuration Values

The K9 config exports these environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `HYPATIA_SCANNER_NAME` | `hypatia` | Scanner name |
| `HYPATIA_VERSION` | `1.0.0` | Scanner version |
| `HYPATIA_RULE_PROPOSAL_THRESHOLD` | `5` | Observations for rule proposal |
| `HYPATIA_AUTO_APPROVAL_OBS` | `10` | Observations for auto-approval |
| `HYPATIA_AUTO_APPROVAL_FIXES` | `3` | Fixes for auto-approval |
| `HYPATIA_SHARED_CONTEXT` | (path) | Fleet coordinator shared context |
| `HYPATIA_ECHIDNA_URL` | `http://localhost:8080` | ECHIDNA validation endpoint |
| `HYPATIA_SCAN_TIMEOUT` | `300` | Scan timeout (seconds) |
| `HYPATIA_MAX_FINDINGS` | `1000` | Max findings per repo |
| `HYPATIA_SEVERITY` | `medium` | Default severity filter |
| `HYPATIA_FORMAT` | `json` | Output format |
| `HYPATIA_ENABLED_PATTERNS` | (csv) | Enabled detection patterns |

### 3. Modifying Fleet Configuration

Edit `fleet-config.k9.ncl`:

```nickel
config = {
  # Enable/disable patterns
  scan_patterns = {
    technical_debt = {
      enabled | Bool = false,  # Disable pattern
      ...
    },
  },

  # Adjust thresholds
  learning_loop = {
    rule_proposal_threshold | Number = 10,  # Require 10 observations
    ...
  },

  # Add new supervised repos
  supervised_repos = {
    core = [
      "echidna",
      "new-repo",  # Add repo
      ...
    ],
  },
}
```

Then reload:
```bash
source <(./read-k9-config.sh fleet-config.k9.ncl)
```

### 4. AI Assistant Workflow (a2ml)

The `rules.a2ml` file provides structured instructions for AI assistants:

**When adding new patterns:**
1. Add detection logic to `hypatia-cli.sh`
2. Deploy to fleet
3. Monitor observations
4. Generate Logtalk rule when threshold crossed (5+ obs)
5. Validate with ECHIDNA (90%+ score required)
6. Deploy to `approved-rules/`

**When modifying rules:**
1. Update both Logtalk rule and bash pattern
2. Re-validate with ECHIDNA
3. Document changes in deployment manifest

---

## K9 Configuration Schema

### Top-Level Structure

```nickel
config = {
  scanner           # Scanner metadata
  supervised_repos  # Repos to actively monitor
  scan_patterns     # Detection rules
  learning_loop     # Learning parameters
  integration       # External endpoints
  scan_schedule     # Cron schedules
  output            # Format settings
  performance       # Resource limits
  severity_filters  # Threshold settings
  auto_fix          # Auto-remediation policy
  monitoring        # Metrics and alerts
  features          # Feature flags
}

manifest = {
  deployed_at       # Deployment timestamp
  deployment_id     # Unique deployment ID
  active_rules      # Rule count
  status            # Production status
}
```

### Pattern Configuration

Each pattern in `scan_patterns` has:

```nickel
pattern_name = {
  enabled | Bool               # Enable/disable detection
  severity | String            # info | low | medium | high | critical
  cwe | String                 # CWE identifier (e.g., "CWE-95")
  languages | Array String     # Applicable languages
  auto_fixable | Bool          # Can be auto-remediated?
  auto_fix_script | String     # Script path (if auto_fixable)
  observations | Number        # Current observation count
  fixes_executed | Number      # Fixes executed (if applicable)
  status | String              # active | approved | deprecated
}
```

### Learning Loop Configuration

```nickel
learning_loop = {
  rule_proposal_threshold | Number     # Observations to trigger proposal

  auto_approval_threshold | {
    observations | Number              # Required observations
    successful_fixes | Number          # Required successful fixes
  }

  validation = {
    required_score | Number            # Minimum validation score (0-1)
    validator | String                 # Validator name
    checks | Array String              # Validation checks
  }

  storage = {
    observations_file | String         # JSONL observation log
    auto_fix_history_file | String     # JSONL fix history
    rule_proposals_dir | String        # Proposal directory
    approved_rules_dir | String        # Approved rules directory
  }
}
```

---

## Integration Points

### 1. Fleet Coordinator

**Configuration:**
```nickel
integration.fleet_coordinator = {
  url | String = "tcp://localhost:5000",
  shared_context_path | String = "/path/to/shared-context",
  findings_dir | String = "findings",
  learning_dir | String = "learning",
}
```

**Usage:**
- Hypatia submits findings to `{shared_context_path}/findings/`
- Fleet coordinator aggregates observations
- Learning loop reads from `{shared_context_path}/learning/`

### 2. ECHIDNA Validation

**Configuration:**
```nickel
integration.echidna = {
  url | String = "http://localhost:8080",
  api_version | String = "v1",
  validation_endpoint | String = "/validate",
}
```

**Usage:**
- Submit rule proposals to ECHIDNA for semantic validation
- Require 90%+ validation score for approval
- ECHIDNA checks logical consistency, coverage, interactions

### 3. Robot-Repo-Automaton

**Configuration:**
```nickel
integration.automaton = {
  url | String = "tcp://localhost:5001",
  auto_fix_dir | String = "/path/to/auto-fixes",
  execution_mode | String = "supervised",
}
```

**Usage:**
- Auto-fix scripts placed in `auto_fix_dir`
- Automaton executes fixes when approved
- `supervised` mode requires human approval per fix

---

## Benefits

### 1. Centralized Configuration
- **Before:** Settings scattered across bash scripts, hardcoded values
- **After:** Single `fleet-config.k9.ncl` file, version controlled

### 2. Type Safety
- **Before:** No validation until runtime errors
- **After:** Nickel validates at parse time, catches config errors early

### 3. AI Assistant Integration
- **Before:** Implicit workflows, manual coordination
- **After:** Explicit `rules.a2ml` documentation, automated workflows

### 4. Fleet Coordination
- **Before:** Manual deployment to each repo
- **After:** K9 Hunt leash deploys to entire fleet automatically

### 5. Dynamic Configuration
- **Before:** Edit bash script, re-deploy scanner
- **After:** Edit K9 config, reload environment variables

---

## Example Workflows

### Adding a New Pattern

**1. Add to K9 config:**
```nickel
scan_patterns = {
  new_pattern = {
    enabled | Bool = true,
    severity | String = "high",
    cwe | String = "CWE-XXXX",
    languages | Array String = ["rust"],
    auto_fixable | Bool = false,
    observations | Number = 0,
    status | String = "active",
  },
}
```

**2. Add detection to hypatia-cli.sh:**
```bash
# Pattern N: New Pattern (HIGH)
while IFS=: read -r linenum line; do
    # ... detection logic ...
done < <(rg -n 'pattern' "$file" 2>/dev/null || true)
```

**3. Deploy and monitor:**
```bash
source <(./read-k9-config.sh)
./hypatia-cli.sh scan /path/to/repo
```

**4. When threshold crossed (5 obs), generate rule:**
- Create Logtalk rule in `rule-proposals/`
- Validate with ECHIDNA
- Deploy to `approved-rules/`

### Disabling a Pattern Temporarily

```nickel
scan_patterns = {
  technical_debt = {
    enabled | Bool = false,  # Disable
    ...
  },
}
```

```bash
source <(./read-k9-config.sh)
# Pattern now disabled in scans
```

### Adjusting Learning Thresholds

```nickel
learning_loop = {
  rule_proposal_threshold | Number = 10,  # Increase from 5 to 10
  auto_approval_threshold = {
    observations = 20,     # Increase from 10
    successful_fixes = 5,  # Increase from 3
  },
}
```

---

## Validation

### K9 Config Validation

```bash
# Check syntax
nickel typecheck fleet-config.k9.ncl

# Export to JSON (validates and type-checks)
nickel export --format json fleet-config.k9.ncl | jq .

# Test configuration loading
./read-k9-config.sh fleet-config.k9.ncl && env | grep HYPATIA_
```

### a2ml Validation

The `rules.a2ml` file is human-readable documentation. Validate by:
1. Ensuring workflow instructions are current
2. Checking file paths are correct
3. Verifying metrics match `rule-deployment-status.json`

---

## Migration Guide

### From Hardcoded Config to K9

**Before (hardcoded in hypatia-cli.sh):**
```bash
SEVERITY="medium"
SHARED_CONTEXT="/var/mnt/eclipse/repos/gitbot-fleet/shared-context"
```

**After (read from K9):**
```bash
# Load K9 config
if [[ -f "fleet-config.k9.ncl" ]]; then
    source <(./read-k9-config.sh fleet-config.k9.ncl) || true
fi

# Use env vars (with fallback to defaults)
SEVERITY="${HYPATIA_SEVERITY:-medium}"
SHARED_CONTEXT="${HYPATIA_SHARED_CONTEXT:-/var/mnt/eclipse/repos/gitbot-fleet/shared-context}"
```

---

## Troubleshooting

### K9 Config Not Loading

**Problem:** `read-k9-config.sh` fails silently
**Solution:** Check if Nickel is installed:
```bash
nickel --version
```

If not installed, config will use defaults (graceful degradation).

### Pattern Not Detected

**Problem:** Pattern enabled in K9 but not detecting
**Solution:** Check pattern is also implemented in `hypatia-cli.sh`
```bash
grep -A10 "Pattern.*new_pattern" hypatia-cli.sh
```

### Validation Failing

**Problem:** ECHIDNA validation < 90%
**Solution:** Check validation report for specific failures:
```bash
cat /tmp/echidna-validation-report-*.md
```

---

## Future Enhancements

### Planned Features

1. **Dynamic Pattern Loading**
   - Load patterns from K9 config without editing bash script
   - Generate ripgrep patterns from Logtalk rules

2. **Auto-fix Script Generation**
   - Generate fix scripts from Logtalk fix suggestions
   - Template-based script creation

3. **Fleet-wide Metrics Dashboard**
   - Real-time observation counts per pattern
   - False positive rate tracking
   - Auto-approval progress visualization

4. **K9 Deployment Automation**
   - Deploy hypatia to repos via K9 Hunt leash
   - Coordinate with gitbot-fleet K9 components
   - Automated rollback on validation failures

---

## References

- **K9 Specification:** See bundeno/contractiles/k9/
- **a2ml Format:** See echidna/AI.a2ml
- **Fleet Coordinator:** See gitbot-fleet/fleet-coordinator.sh
- **ECHIDNA Validation:** See echidna/src/rust/main.rs

---

**Last Updated:** 2026-02-06T22:45:00+00:00
**Maintained By:** Hypatia Development Team
**Status:** ✅ Production Ready
