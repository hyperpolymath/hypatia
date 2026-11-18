#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Contract of hypatia-cli.sh around the escript it runs: callers see what
# `exec` used to give them, plus a named report when the Erlang VM crashes.
#
#   1. stdout and the exit code pass through unchanged (0 and 3), with no
#      report on stderr;
#   2. stdin reaches the escript;
#   3. an escript killed by SIGSEGV: a report on stderr names the signal,
#      stdout stays empty, and the wrapper itself dies by SIGSEGV (exit 139),
#      so the caller's shell still prints "Segmentation fault … hypatia-cli.sh"
#      for the `toolchain_segfault` triage rule;
#   4. SIGTERM sent to the wrapper reaches the escript, and the escript's
#      own exit code comes back (no orphaned scan);
#   5. an escript killed by SIGTERM: the wrapper dies by SIGTERM (143), with
#      no crash report.
#
# Each case runs a copy of hypatia-cli.sh next to a fake `hypatia` escript.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf -- "${work}"' EXIT
fail=0

pass() { printf 'PASS: %s\n' "$1"; }
bad() { printf 'FAIL: %s\n' "$1" >&2; fail=1; }

# setup_case NAME BODY: a directory holding hypatia-cli.sh and a fake escript
# whose script body is BODY. Prints the wrapper path.
setup_case() {
    local dir="${work}/$1"
    mkdir -p "${dir}"
    cp "${root}/hypatia-cli.sh" "${dir}/hypatia-cli.sh"
    printf '#!/usr/bin/env bash\n%s\n' "$2" > "${dir}/hypatia"
    chmod +x "${dir}/hypatia-cli.sh" "${dir}/hypatia"
    printf '%s\n' "${dir}/hypatia-cli.sh"
}

export HYPATIA_NO_STALE_REBUILD=1

# 1. Pass-through of stdout and exit codes.
w="$(setup_case ok 'echo "[]"; exit 0')"
out="$("${w}" scan . 2>"${work}/ok.err")" && rc=0 || rc=$?
[[ "${rc}" == 0 && "${out}" == "[]" && ! -s "${work}/ok.err" ]] \
    && pass "exit 0 and stdout pass through, no report" \
    || bad "exit 0 case: rc=${rc} out=${out} err=$(cat "${work}/ok.err")"

w="$(setup_case three 'exit 3')"
"${w}" secret-alerts >/dev/null 2>"${work}/three.err" && rc=0 || rc=$?
[[ "${rc}" == 3 && ! -s "${work}/three.err" ]] \
    && pass "exit 3 passes through, no report" \
    || bad "exit 3 case: rc=${rc} err=$(cat "${work}/three.err")"

# 2. stdin reaches the escript.
w="$(setup_case stdin 'cat')"
out="$(printf 'hello\n' | "${w}")"
[[ "${out}" == "hello" ]] && pass "stdin reaches the escript" || bad "stdin case: out=${out}"

# 3. A VM crash: named on stderr, and the wrapper dies by the same signal.
w="$(setup_case segv 'kill -SEGV $$')"
outer="$(bash -c '"$1" scan . > "$2" 2> "$3"; echo "rc=$?"' _ "${w}" "${work}/segv.out" "${work}/segv.err" 2>"${work}/segv.outer")"
if [[ "${outer}" == "rc=139" ]] \
    && grep -q 'beam.smp) was killed by SIGSEGV (exit 139)' "${work}/segv.err" \
    && [[ ! -s "${work}/segv.out" ]] \
    && grep -q 'Segmentation fault' "${work}/segv.outer"; then
    pass "SIGSEGV: reported on stderr, stdout empty, wrapper dies by SIGSEGV (139)"
else
    bad "SIGSEGV case: ${outer}; err=$(cat "${work}/segv.err"); outer stderr=$(cat "${work}/segv.outer")"
fi

# 4. SIGTERM to the wrapper reaches the escript; its exit code comes back.
w="$(setup_case term_handled 'trap "exit 42" TERM; sleep 30 & wait')"
"${w}" scan . 2>"${work}/th.err" & wpid=$!
sleep 1
kill -TERM "${wpid}"
wait "${wpid}" && rc=0 || rc=$?
if [[ "${rc}" == 42 ]] && ! pgrep -f "${work}/term_handled/hypatia" >/dev/null; then
    pass "SIGTERM is forwarded; the escript's exit code (42) comes back"
else
    bad "SIGTERM forwarding case: rc=${rc}"
fi

# 5. An escript killed by SIGTERM: the wrapper dies by SIGTERM, no report.
w="$(setup_case term_default 'exec sleep 30')"
"${w}" scan . 2>"${work}/td.err" & wpid=$!
sleep 1
kill -TERM "${wpid}"
wait "${wpid}" && rc=0 || rc=$?
if [[ "${rc}" == 143 && ! -s "${work}/td.err" ]] && ! pgrep -f "${work}/term_default" >/dev/null; then
    pass "escript killed by SIGTERM: wrapper exits 143, no crash report"
else
    bad "SIGTERM default case: rc=${rc} err=$(cat "${work}/td.err")"
fi

exit "${fail}"
