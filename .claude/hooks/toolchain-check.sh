#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
#
# toolchain-check.sh: tell a new session which hypatia tools are missing.
#
# SessionStart hook. It prints nothing when the tools are present, so it adds
# nothing to the context of a normal session. It always exits 0: a missing
# tool must not stop a session, it must only be visible.

missing=""
for tool in mix erl cargo just; do
  command -v "$tool" >/dev/null 2>&1 || missing="$missing $tool"
done
[ -z "$missing" ] && exit 0

echo "hypatia toolchain: these tools are missing:$missing."
echo "On Ubuntu 24.04 x86_64, run scripts/dev/install-toolchain.sh. On other systems, run: mise install erlang elixir zig"
echo "Without mix, no test ran. Report that the tests did not run. Do not report a result."
exit 0
