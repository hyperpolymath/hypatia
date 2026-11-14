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

1. **Start from the newest `main`.** Run `git fetch origin main`. Find the
   work order in `docs/operations/e0-closeout/work-orders/` on `origin/main`,
   and start the branch that its `:wo-branch:` line names from `origin/main`.
   Do each later step on this branch, so that the code that you measure is
   the code that you change.
2. **Read.** Read the whole work order. Read the "Rules for every run" part
   of `docs/operations/e0-closeout/README.adoc`.
3. **Check the dependencies.** Each id under `:wo-depends:` must be merged in
   `main`. If one is not, stop and say which.
4. **Check the toolchain.** Run `mix --version`. If it fails, run
   `scripts/dev/install-toolchain.sh` when that file exists and the system is
   Ubuntu 24.04. In each other case, use `mise install erlang elixir`.
5. **Measure the facts again.** Open each function that "Facts measured"
   names. Run each command that it names. Line numbers drift, so find the code
   by its function name. If a fact no longer holds, stop and report the
   difference. Do not adapt the work order yourself.
6. **Test first.** Where the work order changes behaviour, write the test,
   run it, and see it fail for the right reason. Then make the change.
7. **Check.** Run, in this order, and copy each result line:
   - the test files that "Acceptance" names;
   - `mix format --check-formatted <file>` for each changed file that was
     formatted before your change, and for each new file (see the note on
     formatting below);
   - `mix test` (about 1 minute);
   - `bash scripts/check-verdicts.sh`, only when you changed `src/Hypatia/ABI/`;
   - `mix escript.build && ./hypatia scan . --format json`, and look for
     findings on the files that you changed.
8. **Gate.** Go through the `estate-gates` skill.
9. **Commit and push.** Commit in the form that part 5 of `estate-gates`
   gives. Then run `git status --short`: it must print nothing, so that the
   commit holds exactly what the checks ran on. Push the branch with
   `git push -u origin <branch>`. Record `git rev-parse HEAD` for the receipts.
10. **Report.** Open one draft pull request. Use
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
  the file. To see how a file was before your change, run
  `git show origin/main:<file> | mix format --check-formatted -`. Exit code 0
  means that it was formatted.

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
