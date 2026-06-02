#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-pmpl-drift.sh — Flip legacy PMPL-1.0-or-later SPDX stamps to repo-category target
# Recipe: recipe-fix-pmpl-drift (confidence: 0.95, auto_fixable: true)
#
# Per 2026-06-02 owner directive:
#   sole-owner default → MPL-2.0
#   son-shared        → AGPL-3.0-or-later (confirmed: idaptik, burble, standards, rattlescript, vcl-ut)
#   palimpsest carve-out → KEEP PMPL (palimpsest-license, palimpsest-plasma; consent-aware-http special)
#   007              → DO NOT TOUCH (ARR)
#   third-party/forks → DO NOT TOUCH
#
# Usage: fix-pmpl-drift.sh <repo-path> [--dry-run] [--file <specific-file>]

set -euo pipefail

REPO_PATH="${1:?Usage: fix-pmpl-drift.sh <repo-path> [--dry-run]}"
DRY_RUN=""
SPECIFIC_FILE=""

shift
while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run) DRY_RUN=1 ;;
    --file) SPECIFIC_FILE="$2"; shift ;;
    *) echo "Unknown arg: $1" >&2; exit 1 ;;
  esac
  shift
done

REPO_NAME=$(basename "$REPO_PATH")

# ─── Carve-out check ───
SKIP_REPOS=(
  "007"
  "palimpsest-license"
  "palimpsest-plasma"
  "game-server-admin"
  "airborne-submarine-squadron"
  "paint-type"
)
for skip in "${SKIP_REPOS[@]}"; do
  if [ "$REPO_NAME" = "$skip" ]; then
    echo "SKIP: $REPO_NAME is on the exclude_repos list (carve-out or third-party)"
    exit 0
  fi
done

# ─── Category classification via LICENSE file ───
LICENSE_PATH="$REPO_PATH/LICENSE"
if [ ! -f "$LICENSE_PATH" ]; then
  echo "WARN: no LICENSE file at $LICENSE_PATH — skipping (cannot classify)"
  exit 0
fi

LIC_HEAD=$(head -1 "$LICENSE_PATH" | tr -d '\n')

if echo "$LIC_HEAD" | grep -q "MPL-2.0"; then
  TARGET="MPL-2.0"
  CATEGORY="sole-owner"
elif echo "$LIC_HEAD" | grep -q "AGPL-3.0"; then
  TARGET="AGPL-3.0-or-later"
  CATEGORY="son-shared"
elif echo "$LIC_HEAD" | grep -q "PMPL-1.0-or-later"; then
  echo "INFO: $REPO_NAME LICENSE is PMPL-1.0-or-later (carve-out or staging) — skipping"
  exit 0
else
  echo "WARN: $REPO_NAME LICENSE is unfamiliar (\"$LIC_HEAD\") — skipping (manual review)"
  exit 0
fi

# ─── Sub-path exclusions ───
EXCLUDE_PATTERNS=(
  "/.git/"
  "/LICENSES/"
  "/rescript-tea/"
  "/rescript-vite/"
  "/affinescript-vite/"
  "/idaptik-rescript13-staging/"
  "/consent-aware-http/"
)
GREP_EXCLUDE=""
for p in "${EXCLUDE_PATTERNS[@]}"; do
  GREP_EXCLUDE+="${GREP_EXCLUDE:+|}$p"
done

# ─── Submodule exclusion (skip submodule paths) ───
SUB_PATHS=""
if [ -f "$REPO_PATH/.gitmodules" ]; then
  while IFS= read -r submodule_path; do
    SUB_PATHS+="${SUB_PATHS:+|}${submodule_path}/"
  done < <(grep "path =" "$REPO_PATH/.gitmodules" | awk '{print $3}')
fi
[ -n "$SUB_PATHS" ] && GREP_EXCLUDE="$GREP_EXCLUDE|$SUB_PATHS"

# ─── Find drift files ───
cd "$REPO_PATH"
if [ -n "$SPECIFIC_FILE" ]; then
  FILES="$SPECIFIC_FILE"
else
  FILES=$(grep -rl "SPDX-License-Identifier: PMPL-1.0-or-later" . 2>/dev/null | grep -vE "$GREP_EXCLUDE" || true)
fi

COUNT=$(echo "$FILES" | grep -c . 2>/dev/null || echo 0)

if [ "$COUNT" -eq 0 ]; then
  echo "OK: $REPO_NAME has no PMPL drift in safe scope"
  exit 0
fi

echo "$REPO_NAME ($CATEGORY → $TARGET): $COUNT files with PMPL drift"

if [ -n "$DRY_RUN" ]; then
  echo "$FILES" | head -20
  [ "$COUNT" -gt 20 ] && echo "... and $((COUNT - 20)) more"
  exit 0
fi

# ─── Flip ───
echo "$FILES" | xargs sed -i "s|SPDX-License-Identifier: PMPL-1.0-or-later|SPDX-License-Identifier: $TARGET|g"

REMAINING=$(grep -rl "SPDX-License-Identifier: PMPL-1.0-or-later" . 2>/dev/null | grep -vE "$GREP_EXCLUDE" | wc -l)
echo "Flipped $COUNT files. Remaining PMPL in scope: $REMAINING."
