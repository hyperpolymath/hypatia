# Plan: Hypatia Learning Loop Redesign + Safety Triangle

## Context

After sharpening panic-attacker and batch-fixing template findings (4277→3230 weak points, 24.5% reduction), the remaining 3230 findings across 294 repos need systematic remediation. The current learning loop is broken:

- **Hypatia's pattern_analyzer.ex** is 65 lines that write Logtalk facts to `/tmp` with a TODO for actual integration
- **verisimdb-data index.json** is flat: `repo→{last_scan, weak_points}` — no pattern identity, no fix recipes, no outcomes
- **Fleet dispatcher** has 10 finding types but `execute_graphql()` just logs — never dispatches
- **Learning loop** has 2,194 observations across 7 patterns but only 3 deployed rules and 0 auto-approved
- **Robot-repo-automaton** is PENDING — has fix scripts but no auto_fixable rules reach it

The goal: make hypatia detect patterns, match them to fix recipes (using the safety triangle: eliminate → substitute → control), dispatch to the fleet, execute safe fixes, and learn from outcomes.

### Safety Triangle

```
Priority 1: ELIMINATE   — Remove the hazard entirely (dead code, TODO markers, unused unsafe)
Priority 2: SUBSTITUTE  — Replace with proven-safe alternative (unsafe SQL → SafeSQL from proven repo)
Priority 3: CONTROL     — Add engineering controls (bounds checks, documentation, error handling)
```

### proven Repo Assets

85+ safety modules at `/var/mnt/eclipse/repos/proven/src/Proven/`, 65 language bindings via Zig FFI. Key mappings:
- CommandInjection → SafeCommand, SafeSQL, SafeGit, SafeSSH
- PathTraversal → SafePath (formally proven)
- UnsafeDeserialization → SafeJson (formally proven), SafeYAML, SafeXML
- DynamicCodeExecution → SafeHtml, SafeTemplate, SafeMarkdown
- InsecureProtocol → SafeUrl (formally proven), SafeNetwork (formally proven)
- HardcodedSecret → SafeEnv
- UnsafeCode → SafeBuffer, SafeMath, SafeString

### k9-svc and a2ml Evaluation

**k9-svc**: 3 security tiers (Kennel/Yard/Hunt) could gate remediation aggressiveness per-repo. Good idea but premature — requires k9-svc deployment across repos first. **Defer to future phase.**

**a2ml**: Attested markup could declare per-repo remediation policies. Useful but adds complexity beyond MVP. **Defer — use simple JSON config initially.**

### Containerization Baseline

69 of ~294 repos have containerization (Containerfile/Dockerfile/compose). 26 repos have explicit database containers (postgres, arangodb, redis, surrealdb, etc.). Key ones: hypatia (ArangoDB + Dragonfly), voyage-enterprise-decision-system (SurrealDB + XTDB), gitbot-fleet. This baseline helps prioritize which repos get container-aware scanning later.

---

## Phase 1: Pattern Registry — Expand verisimdb-data Schema

### Problem
verisimdb-data stores flat per-repo scan snapshots. No way to track:
- The same pattern across repos (e.g., "unquoted shell variable" in 150 repos)
- Fix recipes (what to do about each pattern)
- Fix outcomes (did the fix work?)
- Temporal trends (is pattern count going up or down?)

### Changes

**File: `/var/mnt/eclipse/repos/verisimdb-data/patterns/registry.json`** (CREATE)
```json
{
  "patterns": {
    "PA009-shell-unquoted-var": {
      "id": "PA009-shell-unquoted-var",
      "category": "CommandInjection",
      "severity": "High",
      "description": "Unquoted shell variable in command context",
      "pa_rule": "PA009",
      "occurrences": 847,
      "repos_affected": 156,
      "first_seen": "2026-02-08T00:00:00Z",
      "last_seen": "2026-02-12T00:00:00Z",
      "trend": "decreasing",
      "triangle_tier": "eliminate",
      "recipe_id": "recipe-shell-quote-vars"
    }
  }
}
```

Pattern IDs follow: `PA{rule_id}-{short-description}` (e.g., PA009-shell-unquoted-var, PA004-rust-transmute, PA016-path-traversal-dotdot).

**File: `/var/mnt/eclipse/repos/verisimdb-data/recipes/` directory** (CREATE)

Each recipe is a JSON file:
```json
{
  "id": "recipe-shell-quote-vars",
  "triangle_tier": "eliminate",
  "pattern_ids": ["PA009-shell-unquoted-var"],
  "description": "Quote all shell variable expansions with ${VAR}",
  "action": "substitute_text",
  "match": "$VAR in command context without quotes",
  "replacement": "\"${VAR}\"",
  "confidence": 0.95,
  "proven_module": null,
  "languages": ["shell"],
  "auto_fixable": true,
  "fix_script": "fix-shell-quoting.sh",
  "successful_fixes": 380,
  "failed_fixes": 0,
  "total_attempts": 380
}
```

Triangle tier mapping for recipes:
- `eliminate`: Pattern can be removed entirely (dead code, TODO→FILL, believe_me removal)
- `substitute`: Pattern replaced with proven module call (unsafe SQL → proven SafeSQL)
- `control`: Pattern remains but is guarded (add documentation, bounds check, error handling)

**File: `/var/mnt/eclipse/repos/verisimdb-data/outcomes/` directory** (CREATE)

Append-only JSONL per month:
```json
{"pattern_id":"PA009-shell-unquoted-var","recipe_id":"recipe-shell-quote-vars","repo":"branch-newspaper","file":"ci-scripts/mirror-push.sh","outcome":"success","timestamp":"2026-02-12T10:00:00Z","bot":"robot-repo-automaton"}
```

**File: `/var/mnt/eclipse/repos/verisimdb-data/index.json`** (MODIFY)

Add fields to repo entries:
```json
{
  "repos": {
    "echidna": {
      "last_scan": "...",
      "weak_points": 15,
      "summary": null,
      "pattern_ids": ["PA004-rust-transmute", "PA005-rust-unwrap"],
      "fixes_applied": 0,
      "fixes_pending": 3,
      "triangle_status": {"eliminate": 0, "substitute": 2, "control": 1}
    }
  }
}
```

### Files to Create/Modify
| File | Action |
|------|--------|
| `verisimdb-data/patterns/registry.json` | CREATE — canonical pattern registry |
| `verisimdb-data/recipes/` | CREATE dir + initial recipe files |
| `verisimdb-data/outcomes/` | CREATE dir + schema |
| `verisimdb-data/index.json` | MODIFY — add pattern_ids, fixes fields |
| `verisimdb-data/scripts/ingest-scan.sh` | MODIFY — populate pattern registry on ingest |

---

## Phase 2: WeakPoint → proven Mapping Table

### Problem
No connection between panic-attacker's 20 WeakPointCategories and proven's 85+ safety modules.

### Changes

**File: `/var/mnt/eclipse/repos/verisimdb-data/recipes/proven-substitutions.json`** (CREATE)

Maps each WeakPointCategory to applicable proven modules with triangle tier = substitute:

```json
{
  "substitutions": [
    {
      "pa_rule": "PA009",
      "category": "CommandInjection",
      "proven_modules": ["SafeCommand", "SafeSQL", "SafeGit", "SafeSSH"],
      "confidence": 0.90,
      "formally_proven": false,
      "note": "SafeCommand for shell, SafeSQL for SQL, SafeGit for git ops"
    },
    {
      "pa_rule": "PA010",
      "category": "UnsafeDeserialization",
      "proven_modules": ["SafeJson", "SafeYAML", "SafeXML"],
      "confidence": 0.95,
      "formally_proven": true,
      "note": "SafeJson is formally proven in Idris2"
    },
    {
      "pa_rule": "PA016",
      "category": "PathTraversal",
      "proven_modules": ["SafePath"],
      "confidence": 0.98,
      "formally_proven": true,
      "note": "Dependent types prove no .. escapes"
    }
  ]
}
```

Full mapping (20 categories):

| PA Rule | Category | proven Module(s) | Triangle Tier | Confidence |
|---------|----------|-------------------|---------------|------------|
| PA001 | UncheckedAllocation | SafeBuffer | substitute | 0.85 |
| PA002 | UnboundedLoop | — | control | 0.70 |
| PA003 | BlockingIO | — | control | 0.65 |
| PA004 | UnsafeCode | SafeBuffer, SafeMath | substitute | 0.80 |
| PA005 | PanicPath | — | control | 0.75 |
| PA006 | RaceCondition | — | control | 0.60 |
| PA007 | DeadlockPotential | — | control | 0.55 |
| PA008 | ResourceLeak | — | control | 0.70 |
| PA009 | CommandInjection | SafeCommand, SafeSQL, SafeGit | substitute | 0.90 |
| PA010 | UnsafeDeserialization | SafeJson*, SafeYAML, SafeXML | substitute | 0.95 |
| PA011 | DynamicCodeExecution | SafeHtml, SafeTemplate | substitute | 0.85 |
| PA012 | UnsafeFFI | — | control | 0.60 |
| PA013 | AtomExhaustion | — | control | 0.70 |
| PA014 | InsecureProtocol | SafeUrl*, SafeNetwork* | substitute | 0.95 |
| PA015 | ExcessivePermissions | SafeCapability | substitute | 0.80 |
| PA016 | PathTraversal | SafePath* | substitute | 0.98 |
| PA017 | HardcodedSecret | SafeEnv | eliminate | 0.90 |
| PA018 | UncheckedError | — | control | 0.75 |
| PA019 | InfiniteRecursion | — | control | 0.65 |
| PA020 | UnsafeTypeCoercion | — | control | 0.70 |

`*` = formally proven in Idris2 (highest assurance)

Categories with no proven module get `control` tier (add guards/documentation).

### Initial Recipe Files (highest-impact first)

Create recipe files for the patterns we already successfully fixed:
1. `recipe-shell-quote-vars.json` — PA009, eliminate, confidence 0.95 (380 successful fixes)
2. `recipe-remove-believe-me.json` — PA004, eliminate, confidence 0.98 (139 successful fixes)
3. `recipe-heredoc-to-install.json` — PA009, eliminate, confidence 0.95 (380 successful fixes)
4. `recipe-todo-to-fill.json` — PA018, eliminate, confidence 0.99 (411 successful fixes)

---

## Phase 3: Hypatia Intelligence Layer

### Problem
pattern_analyzer.ex is 65 lines with a TODO. Fleet dispatcher logs instead of dispatching. No triangle routing.

### New Modules

**File: `/var/mnt/eclipse/repos/hypatia/lib/pattern_registry.ex`** (CREATE)

Deduplicates findings into canonical patterns:
```elixir
defmodule Hypatia.PatternRegistry do
  # Reads verisimdb-data/patterns/registry.json
  # Groups scan weak_points by (category, description_fingerprint)
  # Assigns pattern IDs: PA{rule}-{fingerprint}
  # Tracks occurrence count per pattern across repos
  # Detects trends: increasing/decreasing/stable

  def sync_from_scans(scans)        # Build/update registry from scan data
  def get_pattern(pattern_id)        # Lookup single pattern
  def patterns_by_category(category) # Filter by PA rule
  def trending_patterns(direction)   # Get increasing/decreasing patterns
  def pattern_occurrences(pattern_id) # Count across all repos
end
```

**File: `/var/mnt/eclipse/repos/hypatia/lib/recipe_matcher.ex`** (CREATE)

Matches patterns to fix recipes:
```elixir
defmodule Hypatia.RecipeMatcher do
  # Reads verisimdb-data/recipes/*.json
  # For a given pattern_id, finds applicable recipe(s)
  # Ranks recipes by confidence score
  # Checks if proven module bindings exist for repo's language

  def find_recipes(pattern_id)       # Get all matching recipes
  def best_recipe(pattern_id, lang)  # Highest-confidence recipe for language
  def recipes_by_tier(tier)          # All eliminate/substitute/control recipes
  def recipe_confidence(recipe_id)   # Current confidence score
end
```

**File: `/var/mnt/eclipse/repos/hypatia/lib/triangle_router.ex`** (CREATE)

Decides action based on safety triangle hierarchy:
```elixir
defmodule Hypatia.TriangleRouter do
  # Safety Triangle: Eliminate > Substitute > Control
  # For each pattern+repo combination:
  # 1. Can we eliminate? (recipe exists, confidence >= 0.90, auto_fixable)
  # 2. Can we substitute? (proven module exists, binding for repo language)
  # 3. Fall back to control (add guards, documentation, manual review)

  def route(pattern_id, repo, language) :: {:eliminate, recipe} | {:substitute, recipe} | {:control, finding}
  def route_batch(patterns, repo, language) :: [routed_actions]

  # Confidence thresholds (from gitbot-fleet learning loop)
  # >= 0.95: auto-execute (robot-repo-automaton)
  # 0.85-0.94: execute with review (rhodibot creates PR)
  # < 0.85: report only (sustainabot advisory)
end
```

**File: `/var/mnt/eclipse/repos/hypatia/lib/outcome_tracker.ex`** (CREATE)

Closes the feedback loop:
```elixir
defmodule Hypatia.OutcomeTracker do
  # Writes to verisimdb-data/outcomes/YYYY-MM.jsonl
  # Updates recipe confidence based on success/failure
  # Feeds gitbot-fleet learning loop (fix-outcomes.jsonl)

  def record_outcome(recipe_id, repo, file, outcome)
  def update_recipe_confidence(recipe_id)  # Recalculate from outcomes
  def confidence_trend(recipe_id)          # Is confidence improving?
end
```

### Modified Modules

**File: `/var/mnt/eclipse/repos/hypatia/lib/pattern_analyzer.ex`** (MODIFY)

Replace the 65-line stub with actual analysis pipeline:
```elixir
def analyze_all_scans do
  scans = VerisimdbConnector.fetch_all_scans()

  # Step 1: Sync pattern registry
  PatternRegistry.sync_from_scans(scans)

  # Step 2: For each pattern, find best recipe via triangle
  patterns = PatternRegistry.all_patterns()
  routed = Enum.map(patterns, fn pattern ->
    TriangleRouter.route(pattern.id, pattern.repos, pattern.languages)
  end)

  # Step 3: Dispatch routed actions to fleet
  Enum.each(routed, &FleetDispatcher.dispatch_routed_action/1)

  # Step 4: Return summary
  {:ok, generate_summary(routed)}
end
```

**File: `/var/mnt/eclipse/repos/hypatia/lib/fleet_dispatcher.ex`** (MODIFY)

Add triangle-aware dispatch function:
```elixir
def dispatch_routed_action({:eliminate, recipe, pattern}) do
  if recipe.confidence >= 0.95 do
    dispatch_to_robot_repo_automaton(%{
      type: :auto_fix_request,
      repo: pattern.repo,
      file: pattern.file,
      issue: pattern.description,
      fix_type: "eliminate",
      confidence: recipe.confidence,
      recipe_id: recipe.id
    })
  else
    dispatch_to_rhodibot(%{
      type: :fix_suggestion,
      repo: pattern.repo,
      file: pattern.file,
      issue: pattern.description,
      suggestion: recipe.description
    })
  end
end

def dispatch_routed_action({:substitute, recipe, pattern}) do
  dispatch_to_rhodibot(%{
    type: :fix_suggestion,
    repo: pattern.repo,
    file: pattern.file,
    issue: pattern.description,
    suggestion: "Replace with proven/#{recipe.proven_module}"
  })
  dispatch_to_echidnabot(%{
    type: :proof_obligation,
    repo: pattern.repo,
    claim: "Substitution with #{recipe.proven_module} preserves behavior",
    context: pattern.description
  })
end

def dispatch_routed_action({:control, finding}) do
  dispatch_to_sustainabot(%{
    type: :eco_score,
    repo: finding.repo,
    score: finding.severity_score,
    details: "Unresolved: #{finding.description} (no safe fix available)"
  })
end
```

### Files to Create/Modify
| File | Action |
|------|--------|
| `hypatia/lib/pattern_registry.ex` | CREATE |
| `hypatia/lib/recipe_matcher.ex` | CREATE |
| `hypatia/lib/triangle_router.ex` | CREATE |
| `hypatia/lib/outcome_tracker.ex` | CREATE |
| `hypatia/lib/pattern_analyzer.ex` | MODIFY — replace stub with pipeline |
| `hypatia/lib/fleet_dispatcher.ex` | MODIFY — add triangle-aware dispatch |
| `hypatia/lib/verisimdb_connector.ex` | MODIFY — read patterns/recipes dirs |

---

## Phase 4: Fleet Integration — Connect the Bots

### Problem
Fleet coordinator and bots are wired up structurally but nothing flows end-to-end. Fix scripts exist in gitbot-fleet but aren't triggered by hypatia.

### Changes

**File: `/var/mnt/eclipse/repos/gitbot-fleet/shared-context/learning/confidence-thresholds.json`** (CREATE)

```json
{
  "auto_execute": {"min_confidence": 0.95, "min_successful_fixes": 3},
  "review_required": {"min_confidence": 0.85, "min_successful_fixes": 1},
  "report_only": {"min_confidence": 0.0},
  "confidence_adjustments": {
    "successful_fix": 0.02,
    "failed_fix": -0.05,
    "false_positive": -0.10,
    "no_activity_90_days": -0.03
  }
}
```

**File: `/var/mnt/eclipse/repos/gitbot-fleet/scripts/fix-proven-substitute.sh`** (CREATE)

Generic script that:
1. Takes repo path, file, pattern, proven_module, language as args
2. Looks up proven binding for language in `/var/mnt/eclipse/repos/proven/bindings/{lang}/`
3. Generates import statement for the proven module
4. Reports what substitution to make (for rhodibot PR creation)

**File: `/var/mnt/eclipse/repos/gitbot-fleet/shared-context/src/triangle.rs`** (CREATE)

Add TriangleTier enum to shared-context so all bots understand it:
```rust
pub enum TriangleTier {
    Eliminate,   // Remove the hazard
    Substitute,  // Replace with proven-safe
    Control,     // Add guards/documentation
}
```

### Modified Files
| File | Action |
|------|--------|
| `gitbot-fleet/shared-context/learning/confidence-thresholds.json` | CREATE |
| `gitbot-fleet/scripts/fix-proven-substitute.sh` | CREATE |
| `gitbot-fleet/shared-context/src/triangle.rs` | CREATE |
| `gitbot-fleet/shared-context/src/lib.rs` | MODIFY — add `pub mod triangle;` |
| `gitbot-fleet/shared-context/src/finding.rs` | MODIFY — add `triangle_tier` field |
| `gitbot-fleet/fleet-coordinator.sh` | MODIFY — add triangle-aware routing in process-findings |

---

## Phase 5: Feedback Loop — Close the Circuit

### Problem
Fix outcomes are recorded but never update confidence. Rules never reach auto-approval threshold.

### Changes

**Modify: `hypatia/lib/outcome_tracker.ex`** — implement confidence updates:
- After successful fix: `recipe.confidence += 0.02` (capped at 0.99)
- After failed fix: `recipe.confidence -= 0.05` (floored at 0.10)
- After false positive: `recipe.confidence -= 0.10`
- Write updated confidence back to `verisimdb-data/recipes/{id}.json`

**Modify: `gitbot-fleet/fleet-coordinator.sh`** — in process-findings function:
- Read confidence-thresholds.json before dispatching
- Gate auto-execution by confidence level
- Record outcomes after fix execution

**Seed the loop with completed fixes:**
- We already have real outcomes: 380 shell-quoting fixes, 139 believe_me removals, 411 TODO→FILL, 9 ci-scripts eval fixes
- Write these as initial outcome records to bootstrap confidence scores
- This immediately gives 4 recipes with high confidence (0.95+)

---

## Phase 6: Populate Initial Data

### Bootstrap with Real Data

Run `panic-attack sweep /var/mnt/eclipse/repos` and process results:
1. Extract unique (category, description_fingerprint) pairs → pattern registry
2. For patterns matching our 4 completed recipes, link them
3. For all other patterns, check proven-substitutions.json for applicable proven modules
4. Generate recipe stubs for the top 20 most-frequent patterns without recipes

This gives us immediate value: hypatia can start routing the 3230 remaining findings through the triangle.

---

## Implementation Order

| Step | Files | Estimated |
|------|-------|-----------|
| 1. verisimdb-data schema expansion | 5 files | First |
| 2. proven substitution mapping | 1 file + recipe files | Second |
| 3. Hypatia modules (registry, matcher, router, tracker) | 4 new + 3 modified | Third |
| 4. Fleet integration (thresholds, triangle type, fix script) | 3 new + 3 modified | Fourth |
| 5. Feedback loop wiring | 2 modified | Fifth |
| 6. Bootstrap data from completed fixes + fresh sweep | Scripts | Last |

## Verification

1. `cd /var/mnt/eclipse/repos/hypatia && mix compile` — all modules compile
2. `mix test` — new module tests pass
3. `panic-attack sweep /var/mnt/eclipse/repos --output /tmp/sweep-verify.json` — fresh scan
4. Run hypatia pipeline: `mix run -e "Hypatia.PatternAnalyzer.analyze_all_scans()"` — patterns detected, recipes matched, actions routed
5. Check `verisimdb-data/patterns/registry.json` — populated with patterns
6. Check `verisimdb-data/outcomes/` — seeded with completed fix data
7. Check `gitbot-fleet/shared-context/learning/fix-outcomes.jsonl` — updated
8. `cd /var/mnt/eclipse/repos/gitbot-fleet && cargo build` — shared-context compiles with TriangleTier

---

## Exploration Summary (Pre-Implementation)

### Current State of Each Repo

**verisimdb-data:**
- 27 scan files across 24 tracked repos
- index.json has flat structure: `repo → {last_scan, weak_points, summary}`
- Scripts: ingest-scan.sh (single repo), scan-all.sh (batch)
- No patterns/, recipes/, or outcomes/ directories yet

**hypatia:**
- Root-level Elixir project (mix.exs with Jason dep) for VeriSimDB integration
- Separate elixir-scanner/ app (v2.0.0) with concurrent scanning + learning GenServer
- lib/pattern_analyzer.ex: 65 lines, writes Logtalk facts to /tmp
- lib/fleet_dispatcher.ex: 10 finding types mapped to bots, but execute_graphql() just logs
- lib/verisimdb_connector.ex: reads scans/, converts to Logtalk facts
- Learning thresholds: 5 observations → proposal, 10 + 3 fixes → auto-approve

**gitbot-fleet:**
- shared-context/: Rust lib with bot/finding/context/health/reporting/state/storage modules
- fleet-coordinator.sh: 677 lines, run-scan/process-findings/generate-rules/status/deploy-bots
- learning/: 3,992 observed patterns, 7 approved rules (3 deployed), 2,194 total observations
- scripts/: fix-unpinned-actions.sh, fix-missing-permissions.sh, fix-missing-spdx.sh, fix-cors-wildcard.sh
- Finding struct has: id, source, rule_id, severity, message, category, file, line, suggestion, fixable, fixed

**proven:**
- 255 Idris2 files, 104+ safety modules (SafeCommand, SafePath, SafeJson, etc.)
- 63 language bindings in bindings/
- Zig FFI bridge in ffi/zig/
- Key formally-proven modules: SafeJson, SafePath, SafeUrl, SafeNetwork
