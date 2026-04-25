#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-pin-go-deps.sh — Ensure Go module dependencies are pinned via go.sum
# Recipe: recipe-pin-dependencies (confidence: 0.85, auto_fixable: true)
#
# go.sum pins all direct and transitive dependencies to exact content hashes.
# It must be committed to the repo for reproducible builds (not gitignored).
# go.work.sum is also checked for multi-module workspaces.
#
# Usage: fix-pin-go-deps.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-pin-go-deps.sh <repo-path>}"
FIXES=0

if [[ ! -f "${REPO}/go.mod" ]]; then
  echo "[fix-pin-go-deps] No go.mod found — not a Go module project"
  exit 0
fi

# Ensure go.sum exists (created by go mod tidy)
if [[ ! -f "${REPO}/go.sum" ]]; then
  if command -v go &>/dev/null; then
    echo "[fix-pin-go-deps] go.sum missing — running go mod tidy"
    (cd "${REPO}" && go mod tidy)
    FIXES=$((FIXES + 1))
    echo "[fix-pin-go-deps] Created go.sum"
  else
    echo "[fix-pin-go-deps] WARN: go.sum missing and 'go' not available — run 'go mod tidy' manually" >&2
  fi
else
  echo "[fix-pin-go-deps] go.sum present — OK"
fi

# Remove go.sum from .gitignore if present
if [[ -f "${REPO}/.gitignore" ]]; then
  if grep -qE '^go\.sum' "${REPO}/.gitignore" 2>/dev/null; then
    sed -i '/^go\.sum/d' "${REPO}/.gitignore"
    echo "[fix-pin-go-deps] Removed go.sum from .gitignore (must be committed for pinned deps)"
    FIXES=$((FIXES + 1))
  fi
fi

# Multi-module workspace: check go.work.sum
if [[ -f "${REPO}/go.work" ]]; then
  if [[ ! -f "${REPO}/go.work.sum" ]]; then
    if command -v go &>/dev/null; then
      echo "[fix-pin-go-deps] go.work.sum missing — running go work sync"
      (cd "${REPO}" && go work sync)
      FIXES=$((FIXES + 1))
    else
      echo "[fix-pin-go-deps] WARN: go.work.sum missing — run 'go work sync' manually" >&2
    fi
  fi
  # Remove go.work.sum from .gitignore if present
  if [[ -f "${REPO}/.gitignore" ]]; then
    if grep -qE '^go\.work\.sum' "${REPO}/.gitignore" 2>/dev/null; then
      sed -i '/^go\.work\.sum/d' "${REPO}/.gitignore"
      echo "[fix-pin-go-deps] Removed go.work.sum from .gitignore"
      FIXES=$((FIXES + 1))
    fi
  fi
fi

echo "[fix-pin-go-deps] Total: ${FIXES} fix(es) applied"
