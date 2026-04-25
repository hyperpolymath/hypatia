#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-pin-rust-deps.sh — Ensure Cargo.lock is committed for binary/application crates
# Recipe: recipe-pin-dependencies (confidence: 0.85, auto_fixable: true)
#
# Cargo.lock records exact dependency versions for reproducible builds.
# Binary crates (src/main.rs or [[bin]] entries) should always commit it.
# Library-only crates conventionally omit it — this script respects that.
#
# Usage: fix-pin-rust-deps.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-pin-rust-deps.sh <repo-path>}"
FIXES=0

if [[ ! -f "${REPO}/Cargo.toml" ]]; then
  echo "[fix-pin-rust-deps] No Cargo.toml found — not a Rust project"
  exit 0
fi

# Detect binary crate: has src/main.rs, [[bin]], or workspace with any main.rs
is_binary_crate() {
  [[ -f "${REPO}/src/main.rs" ]] && return 0
  grep -q '^\[\[bin\]\]' "${REPO}/Cargo.toml" 2>/dev/null && return 0
  if grep -q '^\[workspace\]' "${REPO}/Cargo.toml" 2>/dev/null; then
    find "${REPO}" -name "main.rs" \
      -not -path "*/.git/*" \
      -not -path "*/target/*" \
      | grep -q . && return 0
  fi
  return 1
}

# Ensure Cargo.lock exists
if [[ ! -f "${REPO}/Cargo.lock" ]]; then
  if command -v cargo &>/dev/null; then
    echo "[fix-pin-rust-deps] Cargo.lock missing — running cargo generate-lockfile"
    (cd "${REPO}" && cargo generate-lockfile)
    FIXES=$((FIXES + 1))
    echo "[fix-pin-rust-deps] Created Cargo.lock"
  else
    echo "[fix-pin-rust-deps] WARN: Cargo.lock missing and 'cargo' not available — run 'cargo generate-lockfile' manually" >&2
  fi
else
  echo "[fix-pin-rust-deps] Cargo.lock present — OK"
fi

# For binary crates: ensure Cargo.lock is not excluded from version control
if is_binary_crate; then
  if [[ -f "${REPO}/.gitignore" ]]; then
    if grep -qE '^Cargo\.lock' "${REPO}/.gitignore" 2>/dev/null; then
      sed -i '/^Cargo\.lock/d' "${REPO}/.gitignore"
      echo "[fix-pin-rust-deps] Removed Cargo.lock from .gitignore (binary crate — lock file must be committed)"
      FIXES=$((FIXES + 1))
    fi
  fi
else
  echo "[fix-pin-rust-deps] Library crate — Cargo.lock in .gitignore is conventional (not modifying)"
fi

echo "[fix-pin-rust-deps] Total: ${FIXES} fix(es) applied"
