#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# hypatia-sitrep.sh — Run full pipeline scan and display situation report
#
# Desktop launcher: click to scan all repos and get a sitrep.
# Can also be run from terminal for full output.

set -euo pipefail

# Derive HYPATIA_DIR from script location (relocatable)
HYPATIA_DIR="$(cd "$(dirname "$0")/.." && pwd)"
VERISIMDB_DATA="$HOME/Documents/hyperpolymath-repos/nextgen-databases/verisimdb/verisimdb-data"
KIN_DIR="$HOME/.hypatia/kin"
SITREP_FILE="$VERISIMDB_DATA/health/sitrep.txt"
MANIFEST="$VERISIMDB_DATA/dispatch/pending.jsonl"

# Detect if running from desktop (no terminal)
DESKTOP_MODE=false
if [[ -z "${TERM:-}" || "${TERM:-}" == "dumb" || "${LAUNCHED_FROM_DESKTOP:-}" == "1" ]]; then
    DESKTOP_MODE=true
fi

log() { echo "$*"; }
header() { echo ""; echo "==============================================="; echo "  $*"; echo "==============================================="; }

cd "$HYPATIA_DIR"

START_TIME=$(date +%s)

header "HYPATIA SITREP — $(date -u '+%Y-%m-%d %H:%M:%S UTC')"

# --- 1. Run full pipeline + health in a single BEAM boot ---
log ""
log "Running pipeline against verisimdb-data..."

ALL_OUTPUT=$(mix run -e '
# Run pipeline
Hypatia.PatternAnalyzer.analyze_all_scans()

# Process health
IO.puts("SECTION:HEALTH")
health = Hypatia.Kin.Watchdog.process_health()
Enum.each(health, fn p ->
  status = if p.alive, do: "UP", else: "DOWN"
  IO.puts("  #{status} | #{inspect(p.module)} | #{p.layer}/#{p.importance}")
end)

# Contingency
IO.puts("SECTION:CONTINGENCY")
IO.puts("  Emergency level: #{Hypatia.Kin.Contingency.level()}")

# Gate
IO.puts("SECTION:GATE")
s = Hypatia.Kin.Gate.stats()
IO.puts("  Approved: #{s.approved}")
IO.puts("  Held:     #{s.held}")
IO.puts("  Rejected: #{s.rejected}")
IO.puts("  Deferred: #{s.deferred}")
IO.puts("SECTION:END")
' 2>&1)

PIPELINE_TIME=$(($(date +%s) - START_TIME))

# Extract key metrics from pipeline output
SCANS=$(echo "$ALL_OUTPUT" | grep -oP 'Loaded \K\d+' | head -1 || echo "0")
PATTERNS=$(echo "$ALL_OUTPUT" | grep -oP 'registry synced: \K\d+' | head -1 || echo "0")
TOTAL_ACTIONS=$(echo "$ALL_OUTPUT" | grep -oP 'Pipeline complete: \K\d+' | head -1 || echo "0")
AUTO=$(echo "$ALL_OUTPUT" | grep -oP '\K\d+(?= auto)' | head -1 || echo "0")
REVIEW=$(echo "$ALL_OUTPUT" | grep -oP '\K\d+(?= review)' | head -1 || echo "0")
REPORT=$(echo "$ALL_OUTPUT" | grep -oP '\K\d+(?= report)' | head -1 || echo "0")

header "SCAN RESULTS"
log "  Repos scanned:      $SCANS"
log "  Canonical patterns: $PATTERNS"
log "  Actions dispatched: $TOTAL_ACTIONS"
log "    Auto-execute:     $AUTO"
log "    Review (PR):      $REVIEW"
log "    Report only:      $REPORT"
log "  Pipeline time:      ${PIPELINE_TIME}s"

# --- 2. Process Health ---
header "PROCESS HEALTH"
HEALTH_OUTPUT=$(echo "$ALL_OUTPUT" | sed -n '/SECTION:HEALTH/,/SECTION:/p' | grep -E "^\s+(UP|DOWN)" || true)
echo "$HEALTH_OUTPUT"

UP_COUNT=$(echo "$HEALTH_OUTPUT" | grep -c "UP" || echo "0")
DOWN_COUNT=$(echo "$HEALTH_OUTPUT" | grep -c "DOWN" || true)
DOWN_COUNT="${DOWN_COUNT:-0}"
log ""
log "  $UP_COUNT up, $DOWN_COUNT down"

# --- 3. Kin Sibling Heartbeats ---
header "KIN ECOSYSTEM"
if [[ -d "$KIN_DIR" ]]; then
    for hb in "$KIN_DIR"/*.heartbeat.json; do
        [[ -f "$hb" ]] || continue
        KIN_ID=$(jq -r '.kin_id // "unknown"' "$hb")
        STATUS=$(jq -r '.status // "unknown"' "$hb")
        TS=$(jq -r '.timestamp // "never"' "$hb")
        VERSION=$(jq -r '.version // "?"' "$hb")

        if [[ "$TS" != "never" && "$TS" != "null" ]]; then
            TS_EPOCH=$(date -d "$TS" +%s 2>/dev/null || echo "0")
            NOW_EPOCH=$(date +%s)
            AGE_S=$((NOW_EPOCH - TS_EPOCH))
            if [[ $AGE_S -lt 3600 ]]; then
                AGE="$((AGE_S / 60))m ago"
            elif [[ $AGE_S -lt 86400 ]]; then
                AGE="$((AGE_S / 3600))h ago"
            else
                AGE="$((AGE_S / 86400))d ago"
            fi
        else
            AGE="never"
        fi

        log "  $KIN_ID v$VERSION — $STATUS ($AGE)"
    done
else
    log "  No kin directory found"
fi

# --- 4. Contingency Level ---
header "CONTINGENCY"
CONT_OUTPUT=$(echo "$ALL_OUTPUT" | sed -n '/SECTION:CONTINGENCY/,/SECTION:/p' | grep -v "SECTION:" || true)
echo "$CONT_OUTPUT"
CONT_LEVEL=$(echo "$CONT_OUTPUT" | grep -oP 'level: \K\w+' || true)
CONT_LEVEL="${CONT_LEVEL:-normal}"

# --- 5. Gate Stats ---
header "GATE"
echo "$ALL_OUTPUT" | sed -n '/SECTION:GATE/,/SECTION:END/p' | grep -E "^\s+(Approved|Held|Rejected|Deferred)" || true

# --- 6. Top findings by frequency ---
header "TOP PATTERNS (by repo count)"
if [[ -f "$MANIFEST" ]]; then
    jq -r '.pattern_id' "$MANIFEST" 2>/dev/null | sort | uniq -c | sort -rn | head -10 > /tmp/hypatia-top-patterns.txt || true
    while read -r count pid; do
        printf "  %4d repos  %s\n" "$count" "$pid"
    done < /tmp/hypatia-top-patterns.txt
    rm -f /tmp/hypatia-top-patterns.txt
else
    log "  No dispatch manifest found"
fi

# --- 7. Recipes available ---
header "RECIPES"
RECIPE_COUNT=$(find "$VERISIMDB_DATA/recipes/" -name "*.json" 2>/dev/null | wc -l)
log "  $RECIPE_COUNT fix recipes available"

# --- 8. Recipe Confidence Distribution ---
header "RECIPE CONFIDENCE"
if [[ -d "$VERISIMDB_DATA/recipes" ]]; then
    CONF_99=0; CONF_95=0; CONF_90=0; CONF_BELOW=0
    while read -r conf; do
        # Compare as integer (confidence * 100)
        CONF_INT=$(echo "$conf" | awk '{printf "%d", $1 * 100}')
        if [[ $CONF_INT -ge 99 ]]; then
            CONF_99=$((CONF_99 + 1))
        elif [[ $CONF_INT -ge 95 ]]; then
            CONF_95=$((CONF_95 + 1))
        elif [[ $CONF_INT -ge 90 ]]; then
            CONF_90=$((CONF_90 + 1))
        else
            CONF_BELOW=$((CONF_BELOW + 1))
        fi
    done < <(find "$VERISIMDB_DATA/recipes/" -name "recipe-*.json" -exec jq -r '.confidence // 0' {} \; 2>/dev/null)
    log "  0.99 confidence:   $CONF_99 recipes (auto-execute, proven)"
    log "  0.95+ confidence:  $CONF_95 recipes (auto-execute)"
    log "  0.90+ confidence:  $CONF_90 recipes (near auto-execute)"
    log "  Below 0.90:        $CONF_BELOW recipes (review/report tier)"
fi

# --- 9. Outcomes (learning feedback) ---
header "OUTCOMES"
OUTCOME_COUNT=0
if [[ -d "$VERISIMDB_DATA/outcomes" ]]; then
    # Use wc -l directly on globs to avoid buffering entire JSONL contents
    OUTCOME_COUNT=$(wc -l "$VERISIMDB_DATA/outcomes/"*.jsonl 2>/dev/null | tail -1 | awk '{print $1}' || echo "0")
    OUTCOME_COUNT="${OUTCOME_COUNT:-0}"
    SUCCESS_COUNT=$(grep -c '"success"' "$VERISIMDB_DATA/outcomes/"*.jsonl 2>/dev/null | awk -F: '{s+=$2} END{print s+0}' || echo "0")
    SUCCESS_COUNT="${SUCCESS_COUNT:-0}"
    FAIL_COUNT=$(grep -c '"failure"' "$VERISIMDB_DATA/outcomes/"*.jsonl 2>/dev/null | awk -F: '{s+=$2} END{print s+0}' || echo "0")
    FAIL_COUNT="${FAIL_COUNT:-0}"
    # Outcome key coverage: recipe_id (structured) vs pattern (fleet-import legacy)
    RECIPE_ID_KEYS=$(grep -c '"recipe_id"' "$VERISIMDB_DATA/outcomes/"*.jsonl 2>/dev/null | awk -F: '{s+=$2} END{print s+0}' || echo "0")
    PATTERN_KEYS=$(grep -c '"pattern"' "$VERISIMDB_DATA/outcomes/"*.jsonl 2>/dev/null | awk -F: '{s+=$2} END{print s+0}' || echo "0")
    log "  Total outcomes:  $OUTCOME_COUNT"
    log "  Successes:       $SUCCESS_COUNT"
    log "  Failures:        $FAIL_COUNT"
    log "  Key format:      $RECIPE_ID_KEYS use 'recipe_id', $PATTERN_KEYS use 'pattern'"
else
    log "  No outcomes recorded yet"
fi

# --- Summary ---
TOTAL_TIME=$(($(date +%s) - START_TIME))

header "SUMMARY"
if [[ "$DOWN_COUNT" -gt 0 ]]; then
    log "  STATUS: DEGRADED ($DOWN_COUNT processes down)"
elif [[ "$CONT_LEVEL" != "normal" ]]; then
    log "  STATUS: $CONT_LEVEL"
else
    log "  STATUS: OPERATIONAL"
fi
log "  $SCANS repos | $PATTERNS patterns | $TOTAL_ACTIONS actions | $RECIPE_COUNT recipes"
# Coverage metric: count distinct PA rule categories covered by recipes (not raw pattern count)
PA_CATEGORIES=20
PA_COVERED=$(find "$VERISIMDB_DATA/recipes/" -name "recipe-*.json" \
    -exec jq -r '.pattern_ids[]? // empty' {} \; 2>/dev/null \
    | sed 's/-.*//' | sort -u | wc -l)
SC_COVERED=$(find "$VERISIMDB_DATA/recipes/" -name "recipe-scorecard-*.json" \
    -exec jq -r '.category // empty' {} \; 2>/dev/null \
    | sort -u | wc -l)
TOTAL_CATEGORIES=$((PA_CATEGORIES + SC_COVERED))
TOTAL_COVERED=$((PA_COVERED + SC_COVERED))
log "  Coverage: $RECIPE_COUNT recipes covering $TOTAL_COVERED/$TOTAL_CATEGORIES rule categories (${PA_COVERED}/${PA_CATEGORIES} PA + ${SC_COVERED} Scorecard)"
log "  Total time: ${TOTAL_TIME}s"
log ""
log "  Manifest: $MANIFEST"
log "  Report generated: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"

# Save sitrep to file
mkdir -p "$(dirname "$SITREP_FILE")"
{
    echo "Last sitrep: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
    echo "Scans: $SCANS | Patterns: $PATTERNS | Actions: $TOTAL_ACTIONS"
    echo "Auto: $AUTO | Review: $REVIEW | Report: $REPORT"
    echo "Processes: $UP_COUNT up, $DOWN_COUNT down"
    echo "Contingency: $CONT_LEVEL"
    echo "Outcomes: $OUTCOME_COUNT | Recipes: $RECIPE_COUNT"
    echo "Time: ${TOTAL_TIME}s"
} > "$SITREP_FILE"

# --- Kin heartbeat ---
mkdir -p "$KIN_DIR"
cat > "${KIN_DIR}/hypatia.heartbeat.json" <<HEARTBEAT
{
  "kin_id": "hypatia",
  "role": "intelligence",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "status": "healthy",
  "version": "0.9.0",
  "last_run": {
    "scans": ${SCANS},
    "patterns": ${PATTERNS},
    "actions_dispatched": ${TOTAL_ACTIONS},
    "auto_execute": ${AUTO},
    "review": ${REVIEW},
    "report_only": ${REPORT}
  },
  "errors": [],
  "capabilities": ["scan", "pattern_analysis", "triangle_routing", "dispatch", "neural", "gate_review"]
}
HEARTBEAT

# If launched from desktop, show notification
if [[ "$DESKTOP_MODE" == "true" ]] && command -v notify-send &>/dev/null; then
    notify-send -u normal -t 10000 "Hypatia Sitrep" \
        "$SCANS repos scanned | $PATTERNS patterns | $TOTAL_ACTIONS dispatched | ${TOTAL_TIME}s"
fi

# If launched from desktop, keep terminal open
if [[ "$DESKTOP_MODE" == "true" ]]; then
    log ""
    log "Press Enter to close..."
    read -r
fi
