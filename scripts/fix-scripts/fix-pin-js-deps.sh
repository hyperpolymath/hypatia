#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-pin-js-deps.sh — Ensure Deno lockfile is enabled for reproducible JS builds
# Recipe: recipe-pin-dependencies (confidence: 0.85, auto_fixable: true)
#
# Deno uses deno.lock for reproducible dependency pinning. This script ensures
# the lockfile field is configured in deno.json and the lock file is not gitignored.
# NOTE: npm/Bun/yarn are banned (hyperpolymath policy); Deno is the JS runtime.
#
# Usage: fix-pin-js-deps.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-pin-js-deps.sh <repo-path>}"
FIXES=0

DENO_CONFIG=""
if [[ -f "${REPO}/deno.json" ]]; then
  DENO_CONFIG="${REPO}/deno.json"
elif [[ -f "${REPO}/deno.jsonc" ]]; then
  DENO_CONFIG="${REPO}/deno.jsonc"
fi

if [[ -n "$DENO_CONFIG" ]]; then
  # Add "lock": true to deno.json if not present
  if grep -q '"lock"' "$DENO_CONFIG" 2>/dev/null; then
    echo "[fix-pin-js-deps] Lockfile already configured in ${DENO_CONFIG}"
  else
    if command -v python3 &>/dev/null; then
      python3 - "$DENO_CONFIG" <<'PYEOF'
import json, sys
path = sys.argv[1]
with open(path) as f:
    data = json.load(f)
if "lock" not in data:
    data["lock"] = True
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")
    print(f"[fix-pin-js-deps] Added \"lock\": true to {path}")
PYEOF
      FIXES=$((FIXES + 1))
    else
      echo "[fix-pin-js-deps] WARN: python3 not available — add '\"lock\": true' to ${DENO_CONFIG} manually" >&2
    fi
  fi

  # Ensure deno.lock is not gitignored
  if [[ -f "${REPO}/.gitignore" ]]; then
    if grep -qE '^deno\.lock' "${REPO}/.gitignore" 2>/dev/null; then
      sed -i '/^deno\.lock/d' "${REPO}/.gitignore"
      echo "[fix-pin-js-deps] Removed deno.lock from .gitignore"
      FIXES=$((FIXES + 1))
    fi
  fi

elif [[ -f "${REPO}/package.json" ]]; then
  echo "[fix-pin-js-deps] WARN: Found package.json — npm/Node.js detected" >&2
  echo "[fix-pin-js-deps] NOTE: npm/Bun are banned per hyperpolymath policy; migrate to Deno" >&2
  if [[ ! -f "${REPO}/package-lock.json" ]] && [[ ! -f "${REPO}/yarn.lock" ]]; then
    echo "[fix-pin-js-deps] WARN: No lock file found for package.json — deps are unpinned (fix: migrate to Deno)" >&2
  fi
else
  echo "[fix-pin-js-deps] No Deno or npm config found"
  exit 0
fi

echo "[fix-pin-js-deps] Total: ${FIXES} fix(es) applied"
