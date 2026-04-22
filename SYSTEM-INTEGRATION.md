# Hypatia System Integration - Complete Flow

**Status:** ✅ Fully wired and functional (2026-01-25)

## The Complete System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     AUTONOMOUS LEARNING LOOP                    │
└─────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
         ┌──────────────────────────────────────────┐
         │  1. CODE CHANGES PUSHED TO REPO         │
         └──────────────┬───────────────────────────┘
                        │
                        ▼
         ┌──────────────────────────────────────────┐
         │  2. GITHUB ACTIONS TRIGGER               │
         │     (.github/workflows/hypatia-scan.yml) │
         └──────────────┬───────────────────────────┘
                        │
                        ▼
         ┌──────────────────────────────────────────┐
         │  3. HYPATIA SCANNER RUNS                 │
         │     (hypatia-cli.sh scan .)              │
         │                                           │
         │     Rules from:                           │
         │     • code-safety-lessons.lgt            │
         │     • container-security-lessons.lgt     │
         │     • security-lessons.lgt               │
         │     • learned-rules.lgt (auto-generated) │
         └──────────────┬───────────────────────────┘
                        │
                        ▼
         ┌──────────────────────────────────────────┐
         │  4. FINDINGS SUBMITTED TO FLEET          │
         │     → shared-context/findings/           │
         └──────────────┬───────────────────────────┘
                        │
                        ▼
         ┌──────────────────────────────────────────┐
         │  5. FLEET COORDINATOR PROCESSES          │
         │     (fleet-coordinator.sh process)       │
         │                                           │
         │     Bot execution order:                  │
         │     • rhodibot (structure/policy)        │
         │     • echidnabot (verification)          │
         │     • oikos (sustainability)             │
         │     • glambot (accessibility)            │
         │     • seambot (integration)              │
         │     • finishingbot (quality)             │
         │     • robot-repo-automaton (fixes)       │
         │     • hypatia (coordination)             │
         └──────────────┬───────────────────────────┘
                        │
                        ├─────────────┐
                        │             │
                        ▼             ▼
         ┌──────────────────┐  ┌──────────────────┐
         │  6a. LEARNING    │  │  6b. AUTO-FIX    │
         │      ENGINE      │  │      EXECUTION   │
         │                  │  │                  │
         │  Observes:       │  │  robot-repo-     │
         │  • Patterns      │  │  automaton runs  │
         │  • Frequencies   │  │  fixes based on  │
         │  • Fix outcomes  │  │  known patterns  │
         │                  │  │                  │
         │  After 5 obs.:   │  │  Records:        │
         │  → Propose rule  │  │  • Success rate  │
         │                  │  │  • Failure rate  │
         │  After 10 obs +  │  │                  │
         │  3 successes:    │  │  ← Feeds back    │
         │  → Auto-approve  │  │                  │
         │  → Add to        │  │                  │
         │     learned-     │  │                  │
         │     rules.lgt    │  │                  │
         └──────┬───────────┘  └─────┬────────────┘
                │                    │
                │ New rules          │ Fix results
                │                    │
                └──────────┬─────────┘
                           │
                           ▼
         ┌──────────────────────────────────────────┐
         │  7. CONTINUOUS IMPROVEMENT               │
         │                                           │
         │  • New patterns → New rules              │
         │  • Successful fixes → Higher confidence  │
         │  • Failed fixes → Try alternative        │
         │  • 10+ observations → Auto-add rule      │
         └───────────────────────────────────────────┘
```

## What Was Missing (Before 2026-01-25)

❌ **Execution layer** - Rules existed but nothing ran them
❌ **Scanner implementations** - Predicates were stubs
❌ **Bot coordination** - Architecture defined but not deployed
❌ **Learning loop** - No feedback from fixes to rules
❌ **GitHub integration** - No CI/CD calling hypatia

## What Was Built (2026-01-25)

✅ **code-safety-lessons.lgt** - Rust / (legacy ReScript) / CORS / auth patterns [Logtalk engine retired 2026-04; see commit d27fe4b]
✅ **engine/scanner.lgt** - Actual file reading/pattern matching [same retirement]
✅ **hypatia-cli.sh** - Command-line scanner (ripgrep-based)
✅ **fleet-coordinator.sh** - Bot execution & coordination
✅ **learning_engine.lgt** - Auto-generates rules from patterns [same retirement]
✅ **GitHub Actions template** - CI/CD integration

> **2026-04 update:** The `.lgt` (Logtalk) components were retired;
> rule logic now lives entirely in Elixir under `lib/rules/`. The
> historical entries above are kept for provenance.

## How To Use

### 1. Deploy to a Repository

```bash
# Copy workflow to repo
cp hypatia/.github/workflows/hypatia-scan-template.yml \
   your-repo/.github/workflows/hypatia-scan.yml

# Commit and push
cd your-repo
git add .github/workflows/hypatia-scan.yml
git commit -m "Add Hypatia security scanning"
git push
```

### 2. Run Manual Scan

```bash
# Single file
./hypatia-cli.sh scan src/auth/JWT.res

# Entire repo
./hypatia-cli.sh scan /path/to/your/repos/svalinn

# Generate JSON report
HYPATIA_FORMAT=json ./hypatia-cli.sh scan . > report.json
```

### 3. Pre-commit Hook

```bash
# Add to .git/hooks/pre-commit
#!/bin/bash
/path/to/hypatia/hypatia-cli.sh check-staged || exit 1
```

### 4. Fleet Coordination

```bash
cd gitbot-fleet

# Deploy all bots
./fleet-coordinator.sh deploy-bots

# Run full scan on repo
./fleet-coordinator.sh run-scan /path/to/your/repos/svalinn

# Process pending findings
./fleet-coordinator.sh process-findings

# Check status
./fleet-coordinator.sh status
```

## The Learning Loop

### Observation Phase
1. Scanner detects pattern (e.g., `getExn` on external data)
2. Pattern recorded to `shared-context/learning/observed-patterns.jsonl`
3. Counter increments for this pattern

### Threshold Phase
```
Observations | Action
-------------|-----------------------------------
1-4          | Just observe and count
5            | Generate rule proposal → manual review
10 + 3 fixes | Auto-approve → add to learned-rules.lgt
```

### Auto-Generated Rule Example

After seeing `useUnsafePointer()` 10 times, the learning pipeline in
`lib/rules/learning.ex` auto-generates a rule module at confidence 0.50
and promotes it via the Bayesian update loop once fixes succeed. The
former Logtalk example here described the retired `.lgt` engine; the
Elixir equivalent lives in `lib/rules/code_safety.ex`.

> **Note:** this document still contains residual `.lgt` filenames and
> "learned-rules.lgt"-style references from the earlier Logtalk-era
> architecture. A full rewrite is tracked separately; the rules engine
> is now 100% Elixir (`lib/rules/*.ex`).

## Current Rules Coverage

### Code Safety (code-safety-lessons.lgt)
- ✅ Rust `unwrap()` → panic DoS
- ✅ Rust `unwrap_or(0)` → privilege escalation
- ✅ Rust RwLock `unwrap()` → poison panic
- ✅ ReScript `getExn` → crash DoS (legacy — ReScript retired 2026-04; detection kept for migration audits)
- ✅ ReScript `Obj.magic` → type safety bypass (legacy — same)
- ✅ CORS wildcard `*` → CSRF
- ✅ Unverified JWT decode → auth bypass
- ✅ Dev mode without env check → production bypass
- ✅ Default to root UID → privilege escalation
- ✅ Path traversal patterns
- ✅ Command injection patterns

### Container Security (container-security-lessons.lgt)
- ✅ Environment variable injection
- ✅ Shell variable quoting
- ✅ Allowlist validation
- ✅ Merkle proof verification (RFC 6962)
- ✅ Transparency log verification
- ✅ Permissive mode bypasses
- ✅ Runtime delegation security

### Workflow Security (security-lessons.lgt)
- ✅ Unpinned GitHub Actions
- ✅ Missing permissions declarations
- ✅ Missing SPDX headers
- ✅ Branch protection requirements
- ✅ Dependency vulnerabilities

## Statistics & Monitoring

```bash
# View learning stats
sqlite3 shared-context/hypatia.db \
  "SELECT * FROM observed_patterns WHERE count >= 5"

# See pending rule proposals
ls -la shared-context/learning/rule-proposals/

# Check fix success rates
jq '.[] | select(.success_rate > 0.8)' \
  shared-context/learning/fix-outcomes.json
```

## Next Steps (Optional Enhancements)

### Phase 2: DeepProbLog Integration
- Probabilistic reasoning over patterns
- Confidence scores for rule proposals
- Anomaly detection for new attack patterns

### Phase 3: Cyc Knowledge Base
- Commonsense reasoning about code patterns
- Understanding architectural context
- Detecting semantic vulnerabilities

### Phase 4: Self-Healing
- Automatic PR creation for high-confidence fixes
- A/B testing of fix alternatives
- Rollback on CI failure

## Proof of Concept Results

Scan of svalinn (2026-01-25):
```
✗ 46 unsafe getExn calls (CRITICAL)
✗ 56 Obj.magic type bypasses (HIGH)
✓ 0 CORS issues (fixed 2026-01-25)
✓ 0 auth bypass issues (fixed 2026-01-25)
✓ 0 Rust unwrap issues (not a Rust repo)
```

## Architecture Benefits

1. **Autonomous** - Learns and improves without manual intervention
2. **Incremental** - Starts with basic rules, grows organically
3. **Evidence-based** - Rules generated from actual observations
4. **Self-correcting** - Failed fixes reduce pattern confidence
5. **Explainable** - Every rule has observation count + context
6. **Collaborative** - Bots share findings via gitbot-fleet

## The Fire Alarm Analogy (Complete)

Before:
- 🔔 Fire alarm (rules) exists
- 🚒 Fire department (bots) exists
- 📞 Protocol (bot_integration) exists
- ❌ Nobody watching the alarm
- ❌ Nobody calling 911
- ❌ Trucks never roll out

After:
- ✅ GitHub Actions watch alarm (CI/CD)
- ✅ hypatia-cli detects smoke (scanner)
- ✅ fleet-coordinator dispatches trucks (bot execution)
- ✅ robot-repo-automaton puts out fire (auto-fix)
- ✅ learning_engine improves detection (fewer false alarms)
- ✅ System gets smarter with every fire (autonomous learning)

---

**The system is now fully operational and autonomous.**
