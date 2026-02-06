#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Configuration Reader for Hypatia
#
# Reads fleet-config.k9.ncl and exports configuration as environment variables
# for hypatia-cli.sh to consume

set -euo pipefail

K9_CONFIG="${1:-fleet-config.k9.ncl}"

if [[ ! -f "$K9_CONFIG" ]]; then
    echo "Error: K9 config not found: $K9_CONFIG" >&2
    exit 1
fi

# Check if nickel is installed
if ! command -v nickel &> /dev/null; then
    echo "Warning: nickel not installed, using default config" >&2
    exit 0
fi

# Export configuration to JSON
CONFIG_JSON=$(nickel export --format json "$K9_CONFIG" 2>/dev/null || echo "{}")

if [[ "$CONFIG_JSON" == "{}" ]]; then
    echo "Warning: Failed to parse K9 config, using defaults" >&2
    exit 0
fi

# Export scanner configuration
export HYPATIA_SCANNER_NAME=$(echo "$CONFIG_JSON" | jq -r '.config.scanner.name // "hypatia"')
export HYPATIA_VERSION=$(echo "$CONFIG_JSON" | jq -r '.config.scanner.version // "1.0.0"')

# Export learning loop thresholds
export HYPATIA_RULE_PROPOSAL_THRESHOLD=$(echo "$CONFIG_JSON" | jq -r '.config.learning_loop.rule_proposal_threshold // 5')
export HYPATIA_AUTO_APPROVAL_OBS=$(echo "$CONFIG_JSON" | jq -r '.config.learning_loop.auto_approval_threshold.observations // 10')
export HYPATIA_AUTO_APPROVAL_FIXES=$(echo "$CONFIG_JSON" | jq -r '.config.learning_loop.auto_approval_threshold.successful_fixes // 3')

# Export integration endpoints
export HYPATIA_SHARED_CONTEXT=$(echo "$CONFIG_JSON" | jq -r '.config.integration.fleet_coordinator.shared_context_path // "/var/mnt/eclipse/repos/gitbot-fleet/shared-context"')
export HYPATIA_ECHIDNA_URL=$(echo "$CONFIG_JSON" | jq -r '.config.integration.echidna.url // "http://localhost:8080"')

# Export performance settings
export HYPATIA_SCAN_TIMEOUT=$(echo "$CONFIG_JSON" | jq -r '.config.performance.scan_timeout_seconds // 300')
export HYPATIA_MAX_FINDINGS=$(echo "$CONFIG_JSON" | jq -r '.config.performance.max_findings_per_repo // 1000')

# Export severity filter
export HYPATIA_SEVERITY=$(echo "$CONFIG_JSON" | jq -r '.config.severity_filters.default_threshold // "medium"')

# Export output format
export HYPATIA_FORMAT=$(echo "$CONFIG_JSON" | jq -r '.config.output.default_format // "json"')

# Export enabled patterns
ENABLED_PATTERNS=$(echo "$CONFIG_JSON" | jq -r '.config.scan_patterns | to_entries | map(select(.value.enabled == true) | .key) | join(",")')
export HYPATIA_ENABLED_PATTERNS="$ENABLED_PATTERNS"

echo "✅ K9 configuration loaded successfully" >&2
echo "   Scanner: $HYPATIA_SCANNER_NAME v$HYPATIA_VERSION" >&2
echo "   Enabled patterns: $(echo "$ENABLED_PATTERNS" | tr ',' '\n' | wc -l)" >&2
echo "   Shared context: $HYPATIA_SHARED_CONTEXT" >&2
