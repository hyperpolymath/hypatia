#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
"""
Surgically replace the "Submit findings to gitbot-fleet (Phase 2)" step in a
consuming repo's .github/workflows/hypatia-scan.yml with the canonical fixed
block (Layer-1 propagation of hyperpolymath/hypatia#252).

Idempotent and conservative:
  * Replaces ONLY the Phase-2 step block — every other step (including the
    repo's own critical/high gate) is preserved verbatim, so legitimate
    per-repo divergence is not clobbered.
  * If the Phase-2 block already carries `continue-on-error: true` it is
    treated as already-patched (exit 2, no write).
  * If no Phase-2 block / no workflow file is found, exits 3 (skip).
  * The canonical fragment already carries the conventional blank-line
    separator before the following step, so replacement yields exactly
    one separator regardless of the target's prior style.

Exit codes: 0 patched, 2 already-patched, 3 not-applicable, 1 error.
"""
import sys

STEP_PREFIX = "      - name:"
PHASE2_NAME = "- name: Submit findings to gitbot-fleet (Phase 2)"


def main() -> int:
    if len(sys.argv) != 3:
        sys.stderr.write("usage: patch_phase2.py <workflow.yml> <fragment.yml>\n")
        return 1

    wf_path, frag_path = sys.argv[1], sys.argv[2]

    try:
        with open(wf_path, "r", encoding="utf-8") as fh:
            text = fh.read()
    except FileNotFoundError:
        return 3

    had_trailing_newline = text.endswith("\n")
    lines = text.split("\n")
    if had_trailing_newline:
        lines = lines[:-1]  # drop the empty element from the final newline

    # Locate the Phase-2 step.
    start = next(
        (i for i, ln in enumerate(lines)
         if ln.startswith(STEP_PREFIX) and ln.strip() == PHASE2_NAME),
        None,
    )
    if start is None:
        return 3

    # End = next top-level step, else EOF.
    end = next(
        (j for j in range(start + 1, len(lines))
         if lines[j].startswith(STEP_PREFIX)),
        len(lines),
    )

    block = lines[start:end]
    if any(ln.strip() == "continue-on-error: true" for ln in block):
        return 2  # already patched

    with open(frag_path, "r", encoding="utf-8") as fh:
        frag = fh.read().split("\n")
    # Drop only the trailing-newline artifact; the intentional blank
    # separator line that ends the canonical block is retained.
    if frag and frag[-1] == "":
        frag = frag[:-1]

    new_lines = lines[:start] + frag + lines[end:]

    out = "\n".join(new_lines)
    if had_trailing_newline:
        out += "\n"

    with open(wf_path, "w", encoding="utf-8") as fh:
        fh.write(out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
