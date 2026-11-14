---
# SPDX-License-Identifier: CC-BY-SA-4.0
name: cost-guard
description: Rules that keep an API-billed agent run inside its cost cap, covering the model for each kind of task, effort, context size, delegation and the cost report. Use when planning or running work that is billed to the API credit, when choosing a model or effort for a task, and when a run comes close to its cap.
license: CC-BY-SA-4.0
---

# Keep a run inside its cap

Cost is counted for each completed task, not for each token. A cheap run
that fails still costs its tokens, then the retry.

## The model for each kind of task

| Task | Model | Effort |
|---|---|---|
| Mechanical edits and checks that a command can verify | haiku | medium |
| Implementation where the design is decided | sonnet | medium |
| Review of a diff against a work order | sonnet, or opus for the learning code | medium |
| Design, a ruling packet, work across repositories | opus | high |
| Idris2 and other proof work | opus | high |
| A task where opus at high effort failed, with the failure stated | the most capable model | high |

Do not raise the model or the effort on a feeling. Raise it after a failed
run, and write down what failed. Where a test or a checker can judge the
output, run at the lower setting first and repeat only the failures higher.

Dated prices and the reasons are in `docs/guides/claude-api-agents.adoc`.

## Context is the main cost

Each request sends the whole conversation again. A long run costs more for
each turn than a short one.

- One work order for each session. Start a new session for the next one.
- Read the part of a file that you need. Search before you read.
- Do not paste a long log into the conversation. Filter it with `grep` or
  `tail`, and keep the result line.
- Give a bulky read to a subagent on a cheaper model, and take back the
  conclusion. Each subagent start has a fixed cost, so do this for work that
  is large, not for one lookup.
- Do not change the model or the effort in the middle of a run. That starts
  the prompt cache again.

## The cap

- The work order sets the cap in `:wo-cap-usd:`. The launch script passes it
  to the run. The figure that the run sees is a client estimate, and the
  spend can pass it, so stop early.
- At about 80 percent of the cap, stop new work. Commit what is correct, and
  report what is done and what is open.
- A run that reaches the cap without a passing result is a finding about the
  work order. Report it. Do not start the same run again with a higher cap.

## The cost report

Each run ends with one ledger line and one "Cost" line in the pull request
body. The form is in `docs/operations/e0-closeout/templates/ledger-line.adoc`.

## Work that needs no agent loop

Bulk reading, labelling or judging of many independent items does not need
an agent session. The Message Batches API does it at half price when the
answer can wait. Say so in the plan and do not run such work as a session.
