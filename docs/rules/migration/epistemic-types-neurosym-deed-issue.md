<!-- SPDX-License-Identifier: MPL-2.0 -->
# Issue packet for hyperpolymath/epistemic-types

**Suggested title:** Replace `NEUROSYM.a2ml` with a repo DEED — `report-format = "logtalk"` is retired

**Prepared from:** Hypatia branch `arena/82ed8898-hypatia`; artifact `docs/rules/migration/epistemic-types_chora.deed`; ledger `docs/rules/logtalk-migration.adoc`
**Source pinned:** `.machine_readable/descriptiles/NEUROSYM.a2ml` at `5422cd5` (identical on `main` at audit time)

---

## Summary

This repo's `NEUROSYM.a2ml` declares `report-format = "logtalk"` for Hypatia. That is status drift, not a working dependency: Hypatia's rule engine has been pure Elixir (`lib/rules/*.ex`) since its ADR-001 (2026-04-14), its report formats are `json|text|github|sarif`, and the scan here is disabled (`scan-enabled = false`) in any case.

The estate direction is that this descriptor information lives in the repository's DEED, in the `(neurosym …)` clause defined by standards (`1-formats/deed/spec/abnf/deed.abnf` v1.2.1; mapping `1-formats/deed/mappings/agentic-neurosym-playbook-to-repo-deed.adoc`, family 4). A complete, ready-to-land transcription is attached from Hypatia: `epistemic-types_chora.deed`.

## The change

1. Add **`epistemic-types_chora.deed`** at the repository root, with exactly the content of Hypatia `docs/rules/migration/epistemic-types_chora.deed`.
   - Filename matters: DEED dispatches the `repo-deed` head by the `<stem>_chora.deed` name.
   - If this repo has acquired a repo deed since the audit, do **not** add a second one — merge the single `(neurosym …)` clause from the artifact into the existing deed instead.
2. Delete **`.machine_readable/descriptiles/NEUROSYM.a2ml`** once the deed lints (step 3).
3. Validate with the standards gate and keep the receipt in the landing PR:

   ```bash
   bun /path/to/standards/1-formats/deed/tools/deed_lint.js epistemic-types_chora.deed
   # expected: exit 0
   ```

## Semantic diff (complete)

Everything from the `5422cd5` descriptor is carried over:

- `[metadata]` → `:project`, `:version`, `:status`, `:last-updated`, `:author` (strings verbatim).
- `[hypatia-config]` → `(hypatia :scan-enabled #f :scan-depth standard :report-format "json" :scan-paths ("src/EpistemicTypes") :file-globs ("*.agda"))`. Note DEED booleans are `#t/#f` and `scan-depth` is a symbol, per the mapping.
- All ten `[symbolic-rules]` entries → `(rule :name … :pattern … :severity … :note …)`, patterns converted escape-for-escape so the parsed strings are identical; `[symbolic-rules.notes]` → `(notes :checker-of-record … :safe-flags …)`.
- `[neural-config] enabled=false`, `[ci] enabled=false` → `:enabled #f` clauses.
- Meaningful A2ML comments are preserved as DEED comments.

**The only intentional semantic change:** `:report-format "logtalk"` → `"json"`.

Two non-changes, on purpose:

- The checker-of-record (`agda --no-libraries -i src src/EpistemicTypes/All.agda`) and `--safe --without-K` flags are transcribed as-is. Whether this repo should also adopt the estate's current strict-profile addition `--cubical-compatible` is a *separate* proof-policy reconciliation — please don't bundle it silently into this format migration, and don't read this issue as a proof-grade claim about the Agda sources.
- No scan behaviour changes: the scan remains disabled.

## One standards dependency to be aware of

Family 4 of the standards mapping currently *fails closed* on a populated `[symbolic-rules]` table (it has no ratified row yet — this repo is the motivating case, with ten real rules). The `(symbolic-rules (notes …) (rule …))` shape in the artifact is valid DEED syntax under the ABNF, and a companion standards issue (Hypatia `docs/rules/migration/standards-deed-mapping-issue.md`) proposes ratifying exactly this row. Landing here does not depend on that ratification for syntactic validity, but the shape should be treated as "proposed standard" until the row lands.

## Acceptance criteria

- [ ] `deed_lint.js` exits 0 on the landed deed; receipt recorded in the PR.
- [ ] `grep -ri logtalk` over active (non-historical) files finds no report-format or rule-format claim.
- [ ] `NEUROSYM.a2ml` deleted; nothing references the old path.
- [ ] `just check` (Agda proof check) unaffected and still the soundness floor, as the original notes say.
