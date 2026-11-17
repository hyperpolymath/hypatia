#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
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

# build_escript builds the Hypatia escript in the script directory and sends build output to stderr.

build_escript() {
    echo "[hypatia] Building escript..." >&2
    # Everything below is BUILD CHATTER, never scan output. Callers run
    #     hypatia-cli.sh scan . > hypatia-findings.json
    # so any byte written to stdout here lands inside the JSON payload and
    # `jq` fails with "Invalid numeric literal" -- reported by the caller as
    # "Hypatia did not produce a valid JSON findings array", on a scan that
    # actually succeeded. Observed in CI 2026-09-03.
    #
    # Redirect the whole subshell to stderr ONCE. Do NOT write `2>&1 >&2`
    # inside it: redirections are applied left to right, so `2>&1` first
    # points fd2 at wherever fd1 currently is -- the caller's findings.json --
    # and the following `>&2` then points fd1 at that same file. Both streams
    # end up in the payload, which is the exact opposite of the intent. The
    # bug is invisible interactively, where fd1 is the tty and the pair is a
    # no-op; it only fires when a caller redirects stdout, i.e. in CI.
    (
        cd "${HYPATIA_DIR}"
        mix deps.get --quiet 2>/dev/null || true
        MIX_NO_PUBSUB="${MIX_NO_PUBSUB:-1}" mix escript.build
    ) >&2
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

# ─── Run the escript, naming a VM crash ────────────────────────────────

# A crash signal is the Erlang VM dying, not a scan result. Hypatia loads no
# NIFs, so a SIGSEGV here is the BEAM itself (the known case is the PinSites
# miscompile, #1062). Callers see only "exit 139", and that reads as a
# scanner fault or a finding about the scanned repository.
# report_vm_crash prints the signal and the OTP/erts version to stderr only
# (stdout carries the JSON/SARIF payload).
# otp_version prints the OTP release from the installation's OTP_VERSION
# file and the erts version from `erl +V`; neither starts a shell or
# evaluates code.
otp_version() {
    local erl root otp erts
    erl="$(command -v erl 2>/dev/null)" || { echo unknown; return 0; }
    root="$(cd "$(dirname "$(readlink -f "${erl}")")/.." 2>/dev/null && pwd)" || root=""
    otp="$(cat "${root}"/releases/*/OTP_VERSION 2>/dev/null | head -n 1)" || otp=""
    erts="$(erl +V 2>&1 | sed -n 's/.*emulator version //p')" || erts=""
    echo "${otp:-unknown} (erts ${erts:-unknown})"
}

report_vm_crash() {
    local status="$1" sig name otp
    sig=$(( status - 128 ))
    name="$(kill -l "${sig}" 2>/dev/null || echo "${sig}")"
    otp="$(otp_version)"
    {
        echo "[hypatia] The Erlang VM running the scanner (beam.smp) was killed by SIG${name} (exit ${status}) on OTP ${otp}."
        echo "[hypatia] This is a runtime crash: no scan result was produced, and it is not a finding about the scanned repository."
        echo "[hypatia] Re-running on the same input and runtime crashes again. Report the OTP version above; see ci_failure_triage 'toolchain_segfault'."
    } >&2
}

# run_escript runs the escript as a child so its exit can be read, and stays
# transparent to callers as `exec` was: stdin, stdout and stderr pass
# through, TERM/INT/HUP are forwarded, the exit code is returned unchanged,
# and a child killed by a signal ends this script by the same signal. The
# caller's shell therefore still reports "Segmentation fault … hypatia-cli.sh",
# which the `toolchain_segfault` triage rule matches.
run_escript() {
    local child status=0 sig
    "${ESCRIPT}" "$@" <&0 &
    child=$!
    trap 'kill -TERM "${child}" 2>/dev/null' TERM
    trap 'kill -INT "${child}" 2>/dev/null' INT
    trap 'kill -HUP "${child}" 2>/dev/null' HUP
    # `wait` returns early (>128) when a forwarded signal interrupts it; wait
    # again until the child has actually exited.
    while :; do
        status=0
        wait "${child}" || status=$?
        kill -0 "${child}" 2>/dev/null || break
    done
    trap - TERM INT HUP
    if (( status > 128 )); then
        sig=$(( status - 128 ))
        case "$(kill -l "${sig}" 2>/dev/null)" in
            SEGV|BUS|ABRT|ILL) report_vm_crash "${status}" ;;
        esac
        trap - "${sig}" 2>/dev/null || true
        kill -"${sig}" $$
    fi
    return "${status}"
}

# ─── Forward arguments ─────────────────────────────────────────────────

if [[ -x "${ESCRIPT}" ]]; then
    # Pass HYPATIA_FORMAT / HYPATIA_SEVERITY through the environment
    run_escript "$@"
    exit $?
elif [[ -x "${BASH_FALLBACK}" ]]; then
    echo "[hypatia] Warning: escript not built, falling back to bash scanner." >&2
    exec "${BASH_FALLBACK}" "$@"
else
    echo "[hypatia] Error: No escript and no bash fallback found." >&2
    echo "[hypatia] Run 'mix escript.build' in ${HYPATIA_DIR} to build." >&2
    exit 2
fi
