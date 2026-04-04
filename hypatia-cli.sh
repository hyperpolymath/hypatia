#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# hypatia-cli.sh — Shell wrapper for the Hypatia neurosymbolic scanner escript.
#
# This script locates the compiled escript (hypatia) and forwards all
# arguments to it. If the escript has not been built yet, it attempts to
# build it via `mix escript.build`. If Elixir is unavailable, it falls
# back to the standalone bash scanner (hypatia-cli-bash.sh).
#
# Usage:
#     hypatia-cli.sh scan <path> [--format json|text|github] [--rules ...]
#     hypatia-cli.sh report <path>
#     hypatia-cli.sh version
#     hypatia-cli.sh help
#
# Environment:
#     HYPATIA_FORMAT      Output format override (json, text, github)
#     HYPATIA_SEVERITY    Minimum severity override (critical, high, medium, low)

set -euo pipefail

HYPATIA_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Check for hypatia-v2 first (legacy name from workflow mv), then hypatia (mix escript.build output)
if [[ -x "${HYPATIA_DIR}/hypatia-v2" ]]; then
    ESCRIPT="${HYPATIA_DIR}/hypatia-v2"
elif [[ -x "${HYPATIA_DIR}/hypatia" ]]; then
    ESCRIPT="${HYPATIA_DIR}/hypatia"
else
    ESCRIPT="${HYPATIA_DIR}/hypatia"
fi
BASH_FALLBACK="${HYPATIA_DIR}/hypatia-cli-bash.sh"

# ─── Build escript if missing ───────────────────────────────────────────

build_escript() {
    echo "[hypatia] Building escript..." >&2
    (
        cd "${HYPATIA_DIR}"
        mix deps.get --quiet 2>/dev/null || true
        mix escript.build 2>&1 >&2
    )
}

# ─── Locate or build the escript ────────────────────────────────────────

if [[ ! -x "${ESCRIPT}" ]]; then
    # Try to build if mix is available
    if command -v mix &>/dev/null; then
        build_escript
    fi
fi

# ─── Forward arguments ─────────────────────────────────────────────────

if [[ -x "${ESCRIPT}" ]]; then
    # Pass HYPATIA_FORMAT / HYPATIA_SEVERITY through the environment
    exec "${ESCRIPT}" "$@"
elif [[ -x "${BASH_FALLBACK}" ]]; then
    echo "[hypatia] Warning: escript not built, falling back to bash scanner." >&2
    exec "${BASH_FALLBACK}" "$@"
else
    echo "[hypatia] Error: No escript and no bash fallback found." >&2
    echo "[hypatia] Run 'mix escript.build' in ${HYPATIA_DIR} to build." >&2
    exit 2
fi
