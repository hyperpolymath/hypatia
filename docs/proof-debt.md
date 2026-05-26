<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Proof Debt — hypatia

**Schema**: [hyperpolymath/standards `TRUSTED-BASE-REDUCTION-POLICY.adoc`](https://github.com/hyperpolymath/standards/blob/main/docs/TRUSTED-BASE-REDUCTION-POLICY.adoc) (standards#203).

## Initial inventory

The 2026-05-26 estate proof-debt audit
([standards#195](https://github.com/hyperpolymath/standards/pull/195))
detected **5 soundness-relevant escape hatches** in this
repo. This file is the **initial seed** — every marker starts in §(d)
DEBT and the maintainer triages each into §(a) / §(b) / §(c) / §(d) as
classification proceeds.

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

All 5 markers below start in this section. As the
maintainer classifies each, it should be moved into §(a) / §(b) /
§(c) as appropriate. Markers that genuinely belong in §(d) need a
deadline and an owner.

```
(no markers; check-trusted-base passes already)
```

> If `5` > 30, the list above shows the first 30 only.
> The full list is reproducible via:
>
> ```bash
> bash /path/to/standards/scripts/check-trusted-base.sh .
> ```

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

- [standards#195](https://github.com/hyperpolymath/standards/pull/195) — estate proof-debt audit.
- [standards#203](https://github.com/hyperpolymath/standards/pull/203) — trusted-base reduction policy (the schema this file follows).
- [standards#211](https://github.com/hyperpolymath/standards/pull/211) — `check-trusted-base.sh` CI enforcement.

---

🤖 Initial seed by Claude Code, 2026-05-26.
