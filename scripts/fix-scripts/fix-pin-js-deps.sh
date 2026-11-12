#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-pin-js-deps.sh: ensure JS dependencies are pinned by a committed lockfile
# Recipe: recipe-pin-dependencies (confidence: 0.85, auto_fixable: true)
#
# For a Deno repo it turns on deno.lock (deno.json "lock": true) and makes sure
# deno.lock is not gitignored. For a package.json repo it only reports whether a
# lockfile pins the dependencies; it edits nothing there.
# JS runtime order (estate policy): Bun, then Deno, then pnpm; npm only as a
# last resort. bun.lock, bun.lockb and pnpm-lock.yaml all count as pinned.
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
    if command -v jq &>/dev/null; then
      tmp=$(mktemp)
      trap 'rm -f "$tmp"' EXIT
      # jq cannot parse JSONC — a commented deno.jsonc takes the ERROR branch
      # (skip-with-error is intended; never write garbage back).
      if jq '.lock = true' "$DENO_CONFIG" > "$tmp"; then
        chmod --reference="$DENO_CONFIG" "$tmp"  # mktemp is 0600; keep the file's mode
        mv "$tmp" "$DENO_CONFIG"
        echo "[fix-pin-js-deps] Added \"lock\": true to ${DENO_CONFIG}"
        FIXES=$((FIXES + 1))
      else
        echo "[fix-pin-js-deps] ERROR: jq failed to parse ${DENO_CONFIG}" >&2
      fi
    else
      echo "[fix-pin-js-deps] WARN: jq not available — add '\"lock\": true' to ${DENO_CONFIG} manually" >&2
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
  echo "[fix-pin-js-deps] Found package.json (no Deno config)"
  pinned=""
  for lock in bun.lock bun.lockb pnpm-lock.yaml package-lock.json yarn.lock; do
    [[ -f "${REPO}/${lock}" ]] && { pinned="$lock"; break; }
  done
  if [[ -n "$pinned" ]]; then
    echo "[fix-pin-js-deps] Dependencies pinned by ${pinned}"
  else
    echo "[fix-pin-js-deps] WARN: No lock file found for package.json: deps are unpinned (fix: run 'bun install' and commit bun.lock)" >&2
  fi
  echo "[fix-pin-js-deps] NOTE: JS runtime order is Bun, then Deno, then pnpm; npm only as a last resort" >&2
else
  echo "[fix-pin-js-deps] No Deno or npm config found"
  exit 0
fi

echo "[fix-pin-js-deps] Total: ${FIXES} fix(es) applied"
