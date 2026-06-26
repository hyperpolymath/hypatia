<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# Path-reference drift: a typology, tropology, and discriminant set

Practice dataset derived from Hypatia rule **SD022** ("stale `src/<dir>/`
reference after a directory rename") run against the `hyperpolymath/hypatia`
tree. SD022's premise — *a doc mentions `src/<dir>/` and no such directory
exists, therefore a rename left the doc stale* — is sound for a single-crate
workspace but over-fires in a polyglot, multi-crate, cross-repo estate. Of 20
SD022 hits, **17 are false positives, 2 are genuine drift, 1 is prospective**.
The value here is not the verdicts but the **discriminants**: the decidable
features that separate a real dangling path from its many look-alikes.

## Discriminants (decidable features)

| key | question | computed from |
|---|---|---|
| `resolves_as_src_dir_anywhere` | does any real dir match `**/src/<dir>` or root `src/<dir>`? | tree walk |
| `file_relative_dir_exists` | does `<dir-of-citing-file>/src/<dir>` exist? | tree |
| `text_prefix` / `prefix_first_segment` | what path segment precedes `src/<dir>` in the line? | regex capture |
| `prefix_is_local_topdir` | is that leading segment a real top-level dir of THIS repo? | tree |
| `prefixed_path_exists` | does `<prefix>/src/<dir>` exist? | tree |
| `doc_in_corpus_dir` | is the citing file under an exempt corpus (`.audittraining/`, `test*/`, fixtures)? | path |
| `context_kind` | manifest field, code comment, example command, ASCII tree, prose, table | lexical |

## Typology (what a finding *is*)

| type | meaning | verdict | resolved by |
|---|---|---|---|
| **TP_self_drift** | the repo's OWN manifest/doc asserts a layout that contradicts its tree | TRUE positive | fix the doc |
| **TP_real** | a real non-path defect (e.g. workflow missing `timeout-minutes`) | TRUE positive | fix the source |
| **FP_relative** | path is real but unanchored — crate/doc/manifest-relative | false positive | rule: resolve relative + anywhere |
| **FP_foreign** | cites ANOTHER repo's / example project's path | false positive | rule: foreign prefix; or qualify doc |
| **FP_corpus** | illustrative path inside a training corpus | false positive | rule: skip corpus dirs |
| **FP_example** | an example CLI argument / hypothetical snippet | false positive | fix doc to a real/placeholder path |
| **FP_schematic** | an aspirational "canonical pattern" path, not a literal file | false positive | qualify the doc |
| **PROSPECTIVE** | a planned, not-yet-created path in a design doc | not drift | leave / mark planned |
| **ENVIRONMENTAL** | repo-state signal (stale remote branches), not a tree defect | n/a | prune branches |

## Tropology (the rhetorical *mode* of the reference)

A path string in prose is rarely a literal local path. The modes ("tropes")
that fool a naive matcher:

- **literal_local** — a real path in this tree, anchored (`cli/src/commands/…`).
- **literal_local_relative / _docrelative / _manifestrelative** — real but
  written relative to a crate root, the citing doc's dir, or a Cargo manifest
  (`path = "src/bin/…"`).
- **citational_foreign** — quoting another repo's or example project's path
  (`vcl-ut/src/core/…`, `examples/nestjs/src/i18n/…`, `burble (src/Burble/ABI/)`).
- **exemplary** — an example invocation argument (`scan src/auth/JWT.res`).
- **schematic_aspirational** — an ASCII tree, manifest schema, or "canonical
  pattern" describing an *intended* layout (`src/rust/`, `src/core/Bridge.eph`).
- **prospective** — a path that *will* exist once a planned module lands.
- **environmental** — not a path at all (git/branch state).

## Decision procedure (the corrected rule)

```
flag src/<dir> as drift  ⟺
      NOT resolves_as_src_dir_anywhere
  AND NOT file_relative_dir_exists
  AND NOT prefixed_path_exists
  AND NOT (text_prefix present AND prefix_first_segment ∉ local_top_dirs)   # foreign
  AND NOT doc_in_corpus_dir
```

Everything that survives is either genuine self-drift (fix the doc) or a
schematic/exemplary/prospective trope that the *doc* should disambiguate — the
rule should not silently swallow those, it should make the human look once.

## Counts (this dataset)

- 22 findings total (20 SD022 + 1 workflow + 1 git-state)
- verdict: 17 false-positive · 3 true-positive · 1 prospective · 1 environmental
- 13 SD022 FPs are cleared by the corrected rule (structural); 6 are real doc
  fixes (2 of them genuine `src/rust/` drift); 1 prospective is left annotated.

See `path-reference-drift.dataset.jsonl` for the per-finding labelled records
(raw text, discriminants, trope, type, verdict, recommended action).
