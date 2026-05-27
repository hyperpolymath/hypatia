<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Proof Debt — hypatia

**Schema**: [hyperpolymath/standards `TRUSTED-BASE-REDUCTION-POLICY.adoc`](https://github.com/hyperpolymath/standards/blob/main/docs/TRUSTED-BASE-REDUCTION-POLICY.adoc) (standards#203).

## Initial inventory

The 2026-05-26 estate proof-debt audit
([standards#195](https://github.com/hyperpolymath/standards/pull/195))
reported **5 soundness-relevant escape hatches** in this repo. A
follow-up local run of `check-trusted-base.sh` reports **15** — the
delta (10) is shadowed copies of the same 5 fixtures inside two stale
agent worktrees under `.claude/worktrees/`. The worktrees are
untracked (they do not ship with the repo), so a fresh CI checkout
still sees 5; the 15-count is the worst-case local-tree value the
seed should accept without flagging.

**Marker count (canonical / tracked):** 5.
**Marker count (local-tree max, incl. agent worktrees):** 15.

This file is the **initial seed** — every marker starts in §(d) DEBT
and the maintainer triages each into §(a) / §(b) / §(c) / §(d) as
classification proceeds. This revision corrects the prior seed
(which said "no markers; check-trusted-base passes already") and
brings the count line into agreement with the actual scan output.

## (a) DISCHARGED in this repo

*(None yet — entries move here when the proof lands or the construct
is removed.)*

## (b) BUDGETED — tested with a refutation budget

*(None yet — entries belong here when the construct is at an
extraction boundary and is covered by a documented property-test
budget.)*

## (c) NECESSARY AXIOM

*(None yet — entries belong here when the construct encodes a
metatheoretic assumption that cannot be discharged within the working
logic.)*

## (d) DEBT — actively to be closed

All 15 sites below start in this section. Once classified, each moves
to §(a) / §(b) / §(c). The 5 canonical sites are deliberate scanner
fixtures (each header literally says `DO NOT FIX — this file exists
so the build fails if the rule stops firing`); the preliminary
classification is therefore **PROPERTY-TEST** (the fixture itself is
the property), pending the maintainer's confirmation.

### Canonical / tracked sites (5)

| # | File:line | Kind | Preliminary class |
|---|-----------|------|---|
| 1 | `test/soundness/fixtures/code_safety/admitted.v:7` | coq-axiom-or-admit (`Admitted.`) | PROPERTY-TEST (fixture for `code_safety/admitted` rule) |
| 2 | `test/soundness/fixtures/code_safety/sorry.lean:5` | lean-sorry-or-axiom (`:= by sorry`) | PROPERTY-TEST (fixture for `code_safety/sorry` rule) |
| 3 | `test/soundness/fixtures/code_safety/agda_postulate.agda:5` | agda-postulate (`postulate`) | PROPERTY-TEST (fixture for `code_safety/agda_postulate` rule) |
| 4 | `test/soundness/fixtures/code_safety/believe_me.idr:8` | idris-believe-or-assert (`believe_me Z`) | PROPERTY-TEST (fixture for `code_safety/believe_me` rule) |
| 5 | `test/soundness/fixtures/code_safety/unsafe_coerce.hs:10` | rust-or-hs-unsafe (`unsafeCoerce n`) | PROPERTY-TEST (fixture for `code_safety/unsafe_coerce` rule) |

### Shadowed copies under stale agent worktrees (10)

These are byte-identical copies of the 5 canonical fixtures living
under untracked agent worktrees (`.claude/worktrees/`). They will not
appear on a fresh CI checkout, but local invocations of
`check-trusted-base.sh` count them. Listed here so the count line
matches the scanner output even when worktrees are present. Disposing
of these is a **separate** janitorial step (deleting `.claude/worktrees/`
locally) — not a real proof-debt item.

| # | File:line | Kind | Preliminary class |
|---|-----------|------|---|
| 6 | `.claude/worktrees/agent-a4e5738e280951300/test/soundness/fixtures/code_safety/admitted.v:7` | coq-axiom-or-admit | TBD-DEBT (shadowed copy of #1) |
| 7 | `.claude/worktrees/agent-a4e5738e280951300/test/soundness/fixtures/code_safety/sorry.lean:5` | lean-sorry-or-axiom | TBD-DEBT (shadowed copy of #2) |
| 8 | `.claude/worktrees/agent-a4e5738e280951300/test/soundness/fixtures/code_safety/agda_postulate.agda:5` | agda-postulate | TBD-DEBT (shadowed copy of #3) |
| 9 | `.claude/worktrees/agent-a4e5738e280951300/test/soundness/fixtures/code_safety/believe_me.idr:8` | idris-believe-or-assert | TBD-DEBT (shadowed copy of #4) |
| 10 | `.claude/worktrees/agent-a4e5738e280951300/test/soundness/fixtures/code_safety/unsafe_coerce.hs:10` | rust-or-hs-unsafe | TBD-DEBT (shadowed copy of #5) |
| 11 | `.claude/worktrees/agent-a678c735f94059b5b/test/soundness/fixtures/code_safety/admitted.v:7` | coq-axiom-or-admit | TBD-DEBT (shadowed copy of #1) |
| 12 | `.claude/worktrees/agent-a678c735f94059b5b/test/soundness/fixtures/code_safety/sorry.lean:5` | lean-sorry-or-axiom | TBD-DEBT (shadowed copy of #2) |
| 13 | `.claude/worktrees/agent-a678c735f94059b5b/test/soundness/fixtures/code_safety/agda_postulate.agda:5` | agda-postulate | TBD-DEBT (shadowed copy of #3) |
| 14 | `.claude/worktrees/agent-a678c735f94059b5b/test/soundness/fixtures/code_safety/believe_me.idr:8` | idris-believe-or-assert | TBD-DEBT (shadowed copy of #4) |
| 15 | `.claude/worktrees/agent-a678c735f94059b5b/test/soundness/fixtures/code_safety/unsafe_coerce.hs:10` | rust-or-hs-unsafe | TBD-DEBT (shadowed copy of #5) |

The full list is reproducible via:

```bash
bash /path/to/standards/scripts/check-trusted-base.sh .
```

## Suggested triage process

1. Run `scripts/check-trusted-base.sh` locally; it lists every marker
   with file:line.
2. For each marker, decide:
   - Can this be proven? → §(a) DISCHARGED via a PR that adds the proof.
   - Is this at an FFI / extraction / opaque-primitive boundary? →
     §(b) or §(c). Add a property test and document the refutation
     budget for §(b), or cite the metatheoretic justification for §(c).
   - Is this temporary debt? → §(d) with a deadline.
3. Update this file in the same PR that lands the disposition.
4. The `check-trusted-base` CI job (standards#211) ensures markers
   are never un-annotated AND un-enumerated simultaneously.

## Companion documents

- [standards#195](https://github.com/hyperpolymath/standards/pull/195) — estate proof-debt audit (reported 5; this PR corrects the local-tree max to 15).
- [standards#203](https://github.com/hyperpolymath/standards/pull/203) — trusted-base reduction policy (the schema this file follows).
- [standards#211](https://github.com/hyperpolymath/standards/pull/211) — `check-trusted-base.sh` CI enforcement.

---

🤖 Initial seed by Claude Code, 2026-05-26. Count-correction revision 2026-05-27.
