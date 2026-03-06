#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# bot-accountability.sh — Record bot/scan activity in each repo
#
# Creates .hypatia/activity.jsonl in every repo, appending one line per
# bot visit or scan. This provides auditable proof that the system is
# actually running and making changes.
#
# Usage:
#   ./scripts/bot-accountability.sh record <repo> <bot> <action> [details]
#   ./scripts/bot-accountability.sh report <repo>
#   ./scripts/bot-accountability.sh report-all
#   ./scripts/bot-accountability.sh stale [days]  # repos not visited in N days

set -euo pipefail

REPOS_DIR="${HOME}/Documents/hyperpolymath-repos"
NOW=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

record_activity() {
  local repo="$1"
  local bot="$2"
  local action="$3"
  local details="${4:-}"

  mkdir -p "${repo}/.hypatia"

  # Append to activity log (JSONL format)
  printf '{"timestamp":"%s","bot":"%s","action":"%s","details":"%s"}\n' \
    "$NOW" "$bot" "$action" "$details" >> "${repo}/.hypatia/activity.jsonl"

  # Update last-visit summary
  cat > "${repo}/.hypatia/last-visit.json" <<EOF
{
  "last_visit": "${NOW}",
  "last_bot": "${bot}",
  "last_action": "${action}",
  "visits_total": $(wc -l < "${repo}/.hypatia/activity.jsonl")
}
EOF

  echo "Recorded: ${bot} → ${action} in $(basename "$repo")"
}

report_repo() {
  local repo="$1"
  local activity="${repo}/.hypatia/activity.jsonl"

  if [ ! -f "$activity" ]; then
    echo "$(basename "$repo"): NEVER VISITED"
    return
  fi

  local total
  total=$(wc -l < "$activity")
  local last_line
  last_line=$(tail -1 "$activity")
  local last_bot
  last_bot=$(echo "$last_line" | grep -oP '"bot":"\K[^"]+')
  local last_time
  last_time=$(echo "$last_line" | grep -oP '"timestamp":"\K[^"]+')
  local last_action
  last_action=$(echo "$last_line" | grep -oP '"action":"\K[^"]+')

  # Count by bot
  local bot_counts
  bot_counts=$(grep -oP '"bot":"\K[^"]+' "$activity" | sort | uniq -c | sort -rn | head -5)

  echo "$(basename "$repo"): ${total} visits, last: ${last_bot} (${last_action}) at ${last_time}"
  echo "  Bot breakdown: $(echo "$bot_counts" | tr '\n' ', ')"
}

report_all() {
  local visited=0
  local never=0

  for repo_dir in "${REPOS_DIR}"/*/; do
    [ -d "${repo_dir}/.git" ] || continue

    if [ -f "${repo_dir}/.hypatia/activity.jsonl" ]; then
      report_repo "$repo_dir"
      visited=$((visited + 1))
    else
      echo "$(basename "$repo_dir"): NEVER VISITED"
      never=$((never + 1))
    fi
  done

  echo ""
  echo "Summary: ${visited} repos visited, ${never} NEVER visited"
}

find_stale() {
  local days="${1:-7}"
  local cutoff
  cutoff=$(date -u -d "-${days} days" +%s 2>/dev/null || date -u -v-${days}d +%s)

  echo "Repos not visited in ${days} days:"
  echo ""

  for repo_dir in "${REPOS_DIR}"/*/; do
    [ -d "${repo_dir}/.git" ] || continue

    if [ ! -f "${repo_dir}/.hypatia/activity.jsonl" ]; then
      echo "  $(basename "$repo_dir"): NEVER VISITED"
      continue
    fi

    local last_time
    last_time=$(tail -1 "${repo_dir}/.hypatia/activity.jsonl" | grep -oP '"timestamp":"\K[^"]+')
    local last_epoch
    last_epoch=$(date -u -d "$last_time" +%s 2>/dev/null || echo 0)

    if [ "$last_epoch" -lt "$cutoff" ]; then
      echo "  $(basename "$repo_dir"): last visited ${last_time}"
    fi
  done
}

case "${1:-help}" in
  record)
    record_activity "$2" "$3" "$4" "${5:-}"
    ;;
  report)
    report_repo "$2"
    ;;
  report-all)
    report_all
    ;;
  stale)
    find_stale "${2:-7}"
    ;;
  *)
    echo "Usage:"
    echo "  $0 record <repo-path> <bot-name> <action> [details]"
    echo "  $0 report <repo-path>"
    echo "  $0 report-all"
    echo "  $0 stale [days]"
    echo ""
    echo "Bots: hypatia-scan, finishbot, rhodibot, echidnabot, sustainabot, glambot, seambot, oikos"
    echo "Actions: scan, fix-agpl, fix-pins, fix-permissions, fix-spdx, pr-created, audit, report"
    ;;
esac
