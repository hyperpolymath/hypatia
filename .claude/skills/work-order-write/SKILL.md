---
# SPDX-License-Identifier: CC-BY-SA-4.0
name: work-order-write
description: Write a work order in the estate shape (Goal, Authority, Facts measured, Steps, Acceptance, Stop and ask) from a gap, a finding or an issue. Use when asked to plan, scope or brief work for another agent run, or to turn a defect into a work order.
license: CC-BY-SA-4.0
---

# Write a work order

A work order lets a cheaper model do bounded work correctly. It succeeds when
a reader who has only the work order and the repository can do the task and
can tell when it is done.

Start from `docs/operations/e0-closeout/templates/work-order.adoc`. Keep the
six headings and their order. The `:wo-...:` header lines are read by the
launch script, so keep one value on each line.

## Before you write

1. **Find the authority.** A work order needs a ruling, an issue or a plan
   item that permits it, with a link. If you find none, ask the owner. Do not
   write the work order.
2. **Measure.** Read the code and run the commands yourself. Measure on a
   clean tree (`git status --short` prints nothing), so that the commit that
   you record is the code that you measured. Record the
   commit. For each fact give the path, the function with its arity, and the
   line. Say which facts you read and which you ran. A status document is not
   a measurement.
3. **Cut to one result.** One work order gives one result that fits one pull
   request. If the task needs a decision first, split it: a design work order,
   then an implementation work order that depends on it.

## Size, model and cap

| Size | Meaning | Model | Effort | Cap |
|---|---|---|---|---|
| small | 1 module and its test, or documents with measured facts | sonnet (haiku for mechanical edits that a command can verify) | medium | $3 to $6 |
| medium | 2 or 3 modules, new tests, no open design question | sonnet | medium | $8 to $15 |
| design | a decision with options, no code | opus | high | $10 |
| proof | Idris2 or other proof work | opus | high | $25 |

Use the most capable model only when a run at the tier above failed, and say
what failed. These caps are starting values. Replace them with the measured
cost of similar runs when the ledger has them.

## The six parts

- **Goal.** The result in one or two sentences. Not the activity.
- **Authority.** The link, and the exact words that permit the work.
- **Facts measured.** Commit, path, function, line, and how you measured.
- **Steps.** In order. Step 1 measures the facts again. Where behaviour
  changes, the test comes before the change.
- **Acceptance.** Commands with their expected result, test names, and the
  paths that the diff can touch. A reader must be able to check each line
  without the agent.
- **Stop and ask.** Each design question that the work order does not answer,
  each fact that the work rests on, and the cost cap.

## Check the work order before you hand it over

- No line number stands without its function name.
- No acceptance line says "works" or "verified" without a command.
- Each choice that you made for the executor is stated as a decision, with
  the option that you rejected. Where the choice is the owner's, it is under
  "Stop and ask".
- The work order names what is out of scope when a reader could expect it.
- Add the work order to the table in its folder index.
