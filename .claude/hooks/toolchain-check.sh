#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
#
# toolchain-check.sh: tell a new session what it lacks to run the hypatia checks.
#
# SessionStart hook. It prints nothing when the tools are present and the
# locale is UTF-8, so it adds nothing to the context of a normal session. It
# always exits 0: a missing tool must not stop a session, it must only be visible.

missing=""
for tool in mix erl zig cargo just; do
  command -v "$tool" >/dev/null 2>&1 || missing="$missing $tool"
done
if [ -n "$missing" ]; then
  echo "hypatia toolchain: these tools are missing:$missing."
  echo "mix, erl, zig and just: on Ubuntu 24.04 x86_64, run scripts/dev/install-toolchain.sh when that file exists. On other systems, use mise (mise.toml pins erlang, elixir and zig) and your package manager for just."
  echo "cargo: mise.toml lists rust. Install it with mise or with rustup."
  echo "Without mix, no test ran. Report that the tests did not run. Do not report a result."
fi

# Measured on 2026-11-14: with a locale that is not UTF-8, 1 test of the suite
# fails (test/rules/rsr_conformance_test.exs). It passes with LANG=C.UTF-8.
charmap=$(locale charmap 2>/dev/null) || charmap=""
case "$charmap" in
  UTF-8 | "") ;;
  *) echo "hypatia: the locale is not UTF-8. 1 test of the suite fails then. Use a UTF-8 locale, for example LANG=C.UTF-8." ;;
esac
exit 0
