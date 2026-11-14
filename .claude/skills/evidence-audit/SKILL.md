---
# SPDX-License-Identifier: CC-BY-SA-4.0
name: evidence-audit
description: Compare what a plan, a status document or an issue claims with what the code does today, read-only, with file and line evidence. Use when asked for a plan-versus-reality check, a recon, a status audit, or whether a claim in a document is still true.
license: CC-BY-SA-4.0
---

# Audit claims against the code

The result is a table that a person can check line by line. It changes no file.

## Procedure

1. **Fix the commit.** Record the commit that you read
   (`git rev-parse HEAD`). `main` in this repository moves several times each
   day, so a line number is correct for one commit only. Then run
   `git status --short`. If a tracked file has changes, the files that you
   read are not the files of that commit: name each changed file at the top
   of the report, or read the audited files with `git show <commit>:<path>`.
2. **List the claims.** Turn the document into numbered items. One claim for
   each item. Keep the wording of the document.
3. **Check each claim in the code.** Open the code. Do not accept a second
   document as proof. Measure counts with a command, for example
   `find lib/rules -maxdepth 1 -name '*.ex' | wc -l`.
4. **Record the result** in this form:

   | Item | Status | Evidence | Kind | Note |
   |---|---|---|---|---|
   | W1 | CHANGED | `lib/x.ex:120`, "short quote" | CODE | one line |

   - Status: `AS-CLAIMED`, `CHANGED`, `PARTLY` or `NOT-FOUND`.
   - Evidence: path, line, and a quote of 15 words or fewer.
   - Kind: `CODE` (you read the implementation), `RAN` (you ran a command and
     copied its output) or `DOC` (only a document says so).
5. **Check your own surprising results.** Before you report a claim as
   false, look for the second place that can make it true. Example: a test
   file that no targeted CI step names can still run in a job that runs the
   whole suite.
6. **Report** the table, then at most 8 lines on what is still open.

## Rules

- Read-only. No edit, no commit, no push, no install.
- Do not assume that an old claim is still true, and do not assume that it
  was fixed. Both happen in this repository.
- Say "not checked" for an item that you did not reach. Do not fill it in.
- A `DOC` result is weaker than a `CODE` result. A `CODE` result about
  behaviour is weaker than a `RAN` result. Say which one you have.

## Where the plans are

- The Kautz-ladder ultraplan: `docs/ULTRAPLAN-KAUTZ-LADDER-2026-10-19.adoc` in
  `metadatastician/standards`.
- The Hypatia part: https://github.com/hyperpolymath/hypatia/issues/984
- The E0 work list: `docs/operations/e0-closeout/README.adoc`.
- Known stale claims: `docs/DEBT-REGISTER.adoc`.
