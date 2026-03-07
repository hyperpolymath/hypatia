#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# panll-data-bridge.sh — Generate JSON snapshots for PanLL dashboard panels.
#
# Reads verisimdb-data flat files and neural state to produce JSON that
# PanLL's HypatiaEngine.parseNetworks/parseScans can consume directly.
#
# Usage:
#   panll-data-bridge.sh networks    # Neural network ensemble state
#   panll-data-bridge.sh scans       # Per-repo scan results
#   panll-data-bridge.sh summary     # Aggregate pipeline metrics
#   panll-data-bridge.sh all         # Combined output for full dashboard
#
# Output goes to stdout (JSON). Intended to be served via a local file
# or piped to PanLL's data loader.

set -euo pipefail

VERISIMDB="${VERISIMDB_DATA_PATH:-/var/mnt/eclipse/repos/nextgen-databases/verisimdb/verisimdb-data}"
HYPATIA_STATE="${HYPATIA_STATE_DIR:-/var/mnt/eclipse/repos/hypatia/data/verisimdb/neural-states}"
OUTCOMES_DIR="$VERISIMDB/outcomes"
RECIPES_DIR="$VERISIMDB/recipes"
SCANS_DIR="$VERISIMDB/scans"

# --- Network State ---
generate_networks() {
    local networks='[]'

    # Read neural state files if they exist
    for net in graph_of_trust mixture_of_experts liquid_state_machine echo_state_network radial_neural_network; do
        local state_file="$HYPATIA_STATE/${net}.json"
        local status="offline"
        local confidence="0.0"
        local inference_count="0"
        local version="0.0.0"

        if [[ -f "$state_file" ]]; then
            status="active"
            confidence=$(jq -r '.confidence // .posterior_mean // 0.5' "$state_file" 2>/dev/null || echo "0.5")
            inference_count=$(jq -r '.inference_count // .total_inferences // 0' "$state_file" 2>/dev/null || echo "0")
            version=$(jq -r '.version // "1.0.0"' "$state_file" 2>/dev/null || echo "1.0.0")
        fi

        networks=$(echo "$networks" | jq --arg id "$net" --arg status "$status" \
            --argjson conf "$confidence" --argjson infer "$inference_count" \
            --arg ver "$version" \
            '. + [{id: $id, status: $status, confidence: $conf, inference_count: $infer, version: $ver}]')
    done

    echo "$networks" | jq '.'
}

# --- Scan Results ---
generate_scans() {
    local scans='[]'

    # Read all scan result files
    if [[ -d "$SCANS_DIR" ]]; then
        while IFS= read -r scan_file; do
            local repo_name
            repo_name=$(jq -r '.repo // .repository // ""' "$scan_file" 2>/dev/null || echo "")
            if [[ -z "$repo_name" ]]; then
                repo_name=$(basename "$scan_file" .json | sed 's/^scan-//')
            fi

            local finding_count
            finding_count=$(jq -r '(.weak_points | length) // 0' "$scan_file" 2>/dev/null || echo "0")

            local risk_score
            # Compute risk from weak point count (normalized)
            local total_lines
            total_lines=$(jq -r '.statistics.total_lines // 1000' "$scan_file" 2>/dev/null || echo "1000")
            risk_score=$(echo "scale=3; $finding_count / ($total_lines / 100 + 1)" | bc 2>/dev/null || echo "0.0")
            # Clamp to 1.0
            if (( $(echo "$risk_score > 1.0" | bc -l 2>/dev/null || echo 0) )); then risk_score="1.0"; fi

            local last_scanned
            last_scanned=$(jq -r '.scanned_at // .timestamp // ""' "$scan_file" 2>/dev/null || echo "")

            local passed="true"
            if [[ "$finding_count" -gt 0 ]]; then
                passed="false"
            fi

            scans=$(echo "$scans" | jq --arg name "$repo_name" \
                --argjson risk "$risk_score" --argjson findings "$finding_count" \
                --arg scanned "$last_scanned" --argjson passed "$passed" \
                '. + [{repo_name: $name, risk_score: $risk, finding_count: $findings, quarantine_count: 0, last_scanned: $scanned, passed: $passed}]')
        done < <(find "$SCANS_DIR" -name "*.json" -type f 2>/dev/null | sort)
    fi

    echo "$scans" | jq '.'
}

# --- Summary Metrics ---
generate_summary() {
    local total_repos=0
    local total_outcomes=0
    local total_recipes=0
    local success_count=0
    local failure_count=0

    # Count repos from scans
    if [[ -d "$SCANS_DIR" ]]; then
        total_repos=$(find "$SCANS_DIR" -name "*.json" -type f 2>/dev/null | wc -l)
    fi

    # Count recipes
    if [[ -d "$RECIPES_DIR" ]]; then
        total_recipes=$(find "$RECIPES_DIR" -name "recipe-*.json" -type f 2>/dev/null | wc -l)
    fi

    # Count outcomes
    if [[ -d "$OUTCOMES_DIR" ]]; then
        total_outcomes=$(cat "$OUTCOMES_DIR"/*.jsonl 2>/dev/null | wc -l)
        total_outcomes="${total_outcomes:-0}"
        success_count=$(grep -h '"success"' "$OUTCOMES_DIR"/*.jsonl 2>/dev/null | wc -l)
        success_count="${success_count:-0}"
        failure_count=$(grep -h '"failure"' "$OUTCOMES_DIR"/*.jsonl 2>/dev/null | wc -l)
        failure_count="${failure_count:-0}"
    fi

    jq -n --argjson repos "$total_repos" --argjson recipes "$total_recipes" \
        --argjson outcomes "$total_outcomes" --argjson successes "$success_count" \
        --argjson failures "$failure_count" \
        '{
            total_repos: $repos,
            total_recipes: $recipes,
            total_outcomes: $outcomes,
            success_count: $successes,
            failure_count: $failures,
            success_rate: (if $outcomes > 0 then ($successes / $outcomes * 100 | round) else 0 end)
        }'
}

# --- Main ---
case "${1:-all}" in
    networks)
        generate_networks
        ;;
    scans)
        generate_scans
        ;;
    summary)
        generate_summary
        ;;
    all)
        jq -n --argjson networks "$(generate_networks)" \
              --argjson scans "$(generate_scans)" \
              --argjson summary "$(generate_summary)" \
              '{networks: $networks, scans: $scans, summary: $summary}'
        ;;
    *)
        echo "Usage: $0 {networks|scans|summary|all}" >&2
        exit 1
        ;;
esac
