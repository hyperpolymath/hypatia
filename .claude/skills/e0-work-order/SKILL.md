---
# SPDX-License-Identifier: CC-BY-SA-4.0
name: e0-work-order
description: Run one E0 close-out work order from docs/operations/e0-closeout/work-orders/ through to a draft pull request. Use when asked to run, do or continue a work order with an id such as E0-06.
argument-hint: "[work-order-id]"
license: CC-BY-SA-4.0
---

# Run one E0 work order

A work order is one bounded task: one branch, one draft pull request, one cost
cap. The work order is the specification. This skill is the procedure.

Work order to run: `$ARGUMENTS`

## Procedure

1. **Read.** Read the whole work order in
   `docs/operations/e0-closeout/work-orders/`. Read the "Rules for every run"
   part of `docs/operations/e0-closeout/README.adoc`.
2. **Check the dependencies.** Each id under `:wo-depends:` must be merged in
   `main`. If one is not, stop and say which.
3. **Measure the facts again.** Open each function that "Facts measured"
   names. Run each command that it names. Line numbers drift, so find the code
   by its function name. If a fact no longer holds, stop and report the
   difference. Do not adapt the work order yourself.
4. **Check the toolchain.** Run `mix --version`. If it fails on Ubuntu 24.04,
   run `scripts/dev/install-toolchain.sh`. Elsewhere, use `mise install erlang elixir`.
5. **Branch.** Start the branch named in `:wo-branch:` from the newest `main`.
6. **Test first.** Where the work order changes behaviour, write the test,
   run it, and see it fail for the right reason. Then make the change.
7. **Check.** Run, in this order, and copy each result line:
   - the test files that "Acceptance" names;
   - `mix format --check-formatted <each file that you changed>` (see the
     note on formatting below);
   - `mix test` (about 1 minute);
   - `bash scripts/check-verdicts.sh`, only when you changed `src/Hypatia/ABI/`;
   - `mix escript.build && ./hypatia scan . --format json`, and look for
     findings on the files that you changed.
8. **Gate.** Go through the `estate-gates` skill before you push.
9. **Report.** Open one draft pull request. Use
   `docs/operations/e0-closeout/templates/pr-body.adoc` for the body and the
   `claim-receipt` skill for each claim. Then stop. Do not merge. Do not mark
   the pull request ready.

## Rules that the work orders rely on

- `Hypatia.OutcomeTracker.qualified_outcome_counts/1` and
  `qualified_outcome?/1` are the one gate for learning. Use them. Do not write
  a second rule that decides what counts as evidence.
- A zero exit code is not evidence that a fix worked. A skipped check is not a pass.
- The outcome log is append-only. Do not rewrite or delete a row.
- Keep the diff inside the paths that "Acceptance" lists. A wider diff needs a
  reason in the pull request body.
- Formatting: on 2026-11-14, `mix format --check-formatted` reported 25
  unformatted files on `main`, and no CI job runs it. A file that was
  formatted before your change must stay formatted. Do not reformat a file
  that was not: format only the lines that you add, by hand, in the style of
  the file. Use `git stash` to see how a file was before your change.

## When to stop and ask

Stop at each condition under "Stop and ask" in the work order, and when:

- a design question has no answer in the work order;
- a test that you did not write fails on `main` before your change;
- the cost comes close to `:wo-cap-usd:` and the acceptance checks do not pass.

When you stop, write what you measured, the question, and the options that
you see. Name the option that is most correct in the long term. Do not pick
an answer and continue.

## What you saw and did not change

Put each defect or stale claim outside the work order in the "Not done" part
of the pull request body, in one line. Do not fix it in this run.
