<!-- SPDX-License-Identifier: MPL-2.0 -->
# Issue packet for metadatastician/standards

**Suggested title:** DEED neurosym mapping: retire the `"logtalk"` report-format example and ratify populated `[symbolic-rules]`

**Prepared from:** Hypatia branch `arena/82ed8898-hypatia`; motivating case `hyperpolymath/epistemic-types` (`NEUROSYM.a2ml` @ `5422cd5`); ledger Hypatia `docs/rules/logtalk-migration.adoc`

---

## 1. The `"logtalk"` literal is a retired format presented as the example

`1-formats/deed/mappings/agentic-neurosym-playbook-to-repo-deed.adoc` (family 4) maps:

```
[hypatia-config] report-format → (neurosym (hypatia :report-format "logtalk"))   — verbatim STRING
```

and the template/valid fixtures under `1-formats/deed/tools/fixtures/` reproduce `:report-format "logtalk"`. Two of those fixtures are vendored byte-for-byte into Hypatia's DEED-reader conformance tests (`test/fixtures/deed_egress/`), so the drift now has downstream copies that cannot be corrected locally without falsifying fixture provenance.

Facts:

- Hypatia — the system the `report-format` names — retired Logtalk in ADR-001 (2026-04-14). Its report formats are `json | text | github | sarif`; `json` is canonical (findings: `schema/findings.ncl`). There is no Logtalk output to request.
- Neither `0-canon/rsr/LANGUAGE-POLICY.adoc` nor `3-practice/LANGUAGE-POLICY.adoc` lists Logtalk (or Prolog) as approved.
- At audit, at least two live descriptors carry the value as if current: `epistemic-types/.machine_readable/descriptiles/NEUROSYM.a2ml` and `echidna/echidna-playground/.machine_readable/descriptiles/NEUROSYM.a2ml` (the latter with `scan-enabled = true`).

**Requested:**

1. Change the mapping example to `:report-format "json"`, and add an explicit migration note: a source value of `"logtalk"` is a *retired format*, to be normalised to `"json"` during A2ML→DEED migration, **not** carried across verbatim. (This is the one place where "verbatim STRING" is the wrong rule: verbatim preservation of an unimplementable format is how the drift propagated.)
2. Consider closing the value set in the row (as `scan-depth` already is): `json text github sarif`, string-valued, with `json` canonical.
3. Regenerate/update the affected fixtures and template pair so vendored copies downstream (Hypatia's reader fixtures) can re-sync cleanly.
4. In the same pass, fix any converter behaviour in `1-formats/deed/tools/a2ml_to_deed.py` / `migrate_a2ml_to_deed.sh` that would emit `"logtalk"` from a legacy descriptor without complaint.

## 2. Ratify the populated `[symbolic-rules]` row (currently fail-closed)

Family 4 says populated `[symbolic-rules]` / `[neural-config]` fail closed pending table rows. epistemic-types is a real, minimal motivating case: ten rules, a notes sub-table, `neural-config` present but disabled, plus `[metadata]` fields (`status`, `last-updated`, `author`) and a `[ci]` section the current rows don't cover.

Hypatia's proposed transcription (full artifact: `docs/rules/migration/epistemic-types_chora.deed` in the Hypatia repo) uses only generic DEED clause syntax from `deed.abnf` v1.2.1:

```scheme
(repo-deed
  :schema-version "1.0.0"
  :canonical-name "epistemic-types"
  (neurosym
    :project "epistemic-types"
    :version "0.1.0"
    :status "prototype"
    :last-updated "2026-06-15"
    :author "Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>"
    (hypatia
      :scan-enabled #f
      :scan-depth standard
      :report-format "json"
      :scan-paths ("src/EpistemicTypes")
      :file-globs ("*.agda"))
    (symbolic-rules
      (notes
        :checker-of-record "agda --no-libraries -i src src/EpistemicTypes/All.agda"
        :safe-flags "--safe --without-K")
      (rule :name "no-postulate" :pattern "postulate"
            :severity "critical" :note "…")
      …)
    (neural-config :enabled #f)
    (ci :enabled #f)))
```

**Requested:** add family-4 rows for

- `[symbolic-rules] rules[]` → `(symbolic-rules (rule :name S :pattern S :severity S :note S)…)`;
- `[symbolic-rules.notes]` → `(symbolic-rules (notes :checker-of-record S :safe-flags S))` (or a ruling on where notes belong);
- `[metadata] status/author` and `[ci] enabled` (currently only `version`/`last-updated` have rows);
- and state explicitly which A2ML values, if any, still fail closed after these rows (e.g. populated `[neural-config]` floats — Hypatia's own `NEUROSYM.a2ml` remains unconvertible for that reason and is intentionally left for that ruling).

## 3. Optional estate guard

Hypatia now carries `scripts/check-no-active-logtalk.sh` + an exact-path, shrink-only `.logtalk-allowlist` (no tracked `*.lgt/*.logtalk`; no active config requesting Logtalk loaders/formats). If standards wants this estate-wide, Hypatia's implementation is offered as the seed for a template/CI-snippet ruling rather than each repo inventing its own patterns.

## Acceptance criteria

- [ ] No standards mapping, template, or *valid* fixture presents `"logtalk"` as a current Hypatia report format.
- [ ] The A2ML→DEED tooling normalises or rejects a legacy `"logtalk"` value loudly (never emits it silently).
- [ ] Family-4 rows exist for populated `[symbolic-rules]`, its notes, `[ci]`, and the extra `[metadata]` fields — or an explicit ruling records a different canonical shape, in which case Hypatia's epistemic-types artifact will be regenerated to match.
- [ ] Downstream vendored fixtures (Hypatia `test/fixtures/deed_egress/`) can be re-synced from the corrected standards fixtures.
