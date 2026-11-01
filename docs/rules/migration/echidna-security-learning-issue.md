<!-- SPDX-License-Identifier: MPL-2.0 -->
# Issue packet for hyperpolymath/echidna

**Suggested title:** Retire `rules/security_learning.lgt` — semantics now canonical in Hypatia Elixir + JSON

**Prepared from:** Hypatia branch `arena/82ed8898-hypatia`, `docs/rules/logtalk-migration.adoc` (2026-11-01)

---

## Summary

`rules/security_learning.lgt` is dead, active-looking Logtalk. As of this audit it is the only `.lgt` path in ECHIDNA's recursive tree, there is no Logtalk loader/settings file or other in-tree consumer for it, and ECHIDNA's documented stack is Rust/Julia/Idris2/Zig/Chapel. Its security-alert semantics have now been transcribed clause-by-clause into Hypatia's canonical Elixir rule system, so the file can be removed (or explicitly archived) without losing meaning.

## Where the semantics went

All in `hyperpolymath/hypatia`, module `Hypatia.Rules.SecurityErrors` (`lib/rules/security_errors.ex`):

| Logtalk | Elixir |
|---|---|
| `alert_category/2` (18 facts) | `alert_categories/0`, `alert_category/1` |
| `classify_severity/2` | `issue_severity/1` — including the `low` catch-all, now explicit for `SASTID`, `CITestsID`, `unmaintained` (previously drifting to `:info`) |
| `is_auto_fixable/1` (6 facts) | `auto_fixable?/1` |
| `suggest_fix/2` (13 facts) | `fix_suggestion/1` |
| `get_prevention_workflow/2` | `prevention_workflow/1` |
| `cicd_integration::validate_alert/2` | `validate_alert/1` (total-function extension: returns `nil` fields instead of failing; also folds in category + prevention) |
| `export_rules/1` / `push_rules_to_cicd/0` (`security_rule/4` `.lgt`) | `Mix.Tasks.Hypatia.BatchSecurityScan` now emits `learned_security_rules.json` per `schemas/learned-security-rules.schema.json` |
| dynamic learning predicates | superseded wholesale by `Hypatia.Rules.Learning`; no clause-level equivalent is claimed |

Deliberate divergences, all ledgered in Hypatia `docs/rules/logtalk-migration.adoc`:

- `TokenPermissionsID` / `missing-workflow-permissions` fix text no longer prescribes `permissions: read-all` (Hypatia flags `read-all` itself); granular least-privilege text is canonical.
- `PinnedDependenciesID` fix generalised `@v4` → `@vN`.
- `BranchProtectionID` prevention is `scorecard-enforcer.yml` (long-standing Elixir value), not the Logtalk `branch-protection-enforcer.yml`; `CodeReviewID` gained a prevention mapping with no Logtalk counterpart.
- Wholly unknown alert ids get severity `:info` in Elixir rather than the Logtalk catch-all `low`; all 18 known ids agree exactly.

Proof status, stated exactly: the finite table correspondence is modelled in Hypatia `verification/proofs/agda/LogtalkMigration.agda` and is **AGDA-PENDING (Mathesis Grade 3 target)** pending a strict-profile Agda receipt; Elixir behaviour is pinned by ExUnit tests. Neither is being sold here as more than it is.

## Requested changes in ECHIDNA

1. **Delete `rules/security_learning.lgt`** (preferred). Git history plus the Hypatia ledger preserve it; no replacement `.lgt` should be created. If a local archival copy is required instead, move it to a non-executable archival name (e.g. `docs/archive/security_learning.logtalk.txt`) with a header stating it is superseded by Hypatia `lib/rules/security_errors.ex` and linking the ledger — do not leave an active-looking `.lgt` under `rules/`.
2. **Fix `docs/ECOSYSTEM-INTEGRATION.adoc`**, which currently says the learning engine "Auto-generates Logtalk rules after 5+ observations". Replacement wording: learned patterns are proposed as Hypatia Elixir rule modules (`lib/rules/*.ex`) and/or emitted as learned-security-rules JSON; observation counts are belief-forming evidence, never a soundness claim.
3. **Fix `echidna-playground/.machine_readable/descriptiles/NEUROSYM.a2ml`**: it has `scan-enabled = true` with `report-format = "logtalk"`. Exact replacement value: `report-format = "json"` (Hypatia findings, `schema/findings.ncl`). A full DEED conversion can ride the estate descriptile programme separately; the format value should not wait for it.
4. Sweep the tree for any remaining `.lgt` paths, Logtalk loader/settings references, or `format.*logtalk` settings after the deletion.

## Acceptance criteria

- [ ] No `.lgt` / `.logtalk` file remains in an active path (`git ls-files '*.lgt' '*.logtalk'` empty, or only under an explicitly archival, non-executable name).
- [ ] No document in ECHIDNA describes Logtalk rule generation/output as current behaviour.
- [ ] `echidna-playground` NEUROSYM requests JSON.
- [ ] ECHIDNA docs (or the archival stub) point at Hypatia `docs/rules/logtalk-migration.adoc` as the transcription of record.

## Non-goals

No change to ECHIDNA's prover architecture, rule-learning research direction, or non-Logtalk subsystems. This is solely retirement of a dead format and its stale documentation.
