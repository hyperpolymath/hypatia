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
# Prefer the canonical escript name produced by `mix escript.build` (`hypatia`).
# Keep `hypatia-v2` as a backward-compatible fallback for older deployments.
if [[ -x "${HYPATIA_DIR}/hypatia" ]]; then
    ESCRIPT="${HYPATIA_DIR}/hypatia"
elif [[ -x "${HYPATIA_DIR}/hypatia-v2" ]]; then
    ESCRIPT="${HYPATIA_DIR}/hypatia-v2"
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
        MIX_NO_PUBSUB="${MIX_NO_PUBSUB:-1}" mix escript.build 2>&1 >&2
    )
}

# A stale escript is a SOUNDNESS hazard, not a convenience issue: an
# escript built before a rule/pattern was added silently emits zero
# findings for that whole pattern family (observed: an escript predating
# the Elixir/Erlang/Coq/Lean/Agda/Zig pattern sets passed a textbook
# `System.shell("…#{x}")` injection with "0 findings"). So rebuild when
# the binary is missing *or* older than any tracked rule/CLI source.
# Set HYPATIA_NO_STALE_REBUILD=1 to opt out (e.g. air-gapped deploys
# that ship a known-current binary and have no toolchain).
escript_is_stale() {
    [[ -x "${ESCRIPT}" ]] || return 0
    [[ "${HYPATIA_NO_STALE_REBUILD:-0}" == "1" ]] && return 1
    local newer
    newer=$(find "${HYPATIA_DIR}/lib" "${HYPATIA_DIR}/mix.exs" \
                 -name '*.ex' -o -name '*.exs' 2>/dev/null \
            | while IFS= read -r f; do
                  [[ "$f" -nt "${ESCRIPT}" ]] && { echo stale; break; }
              done)
    [[ -n "$newer" ]]
}

# ─── Locate or build the escript ────────────────────────────────────────

if [[ ! -x "${ESCRIPT}" ]]; then
    # Try to build if mix is available
    if command -v mix &>/dev/null; then
        build_escript
    fi
elif escript_is_stale; then
    if command -v mix &>/dev/null; then
        echo "[hypatia] escript is older than rule sources — rebuilding to avoid silent false negatives." >&2
        build_escript
    else
        echo "[hypatia] WARNING: escript is older than rule sources and mix is unavailable; scan results may omit newer pattern families." >&2
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
