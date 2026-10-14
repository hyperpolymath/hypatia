# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Hypatia findings schema — the canonical contract for what a scan emits.
#
# Two producers, two envelopes, one verdict shape:
#   * the escript (`hypatia-cli.sh scan --format json`) prints a bare array
#     of `Finding`;
#   * the bash fallback (hypatia-cli-bash.sh) prints a `BashReport`: an
#     object with `submission_metadata` and an array of `BashFinding`.
# Both carry the HYP-01 verdict fields — `support`, `evidence`, `level` —
# whose meaning is fixed by src/Hypatia/ABI/Verdict.idr (the Idris2 ABI is
# normative; this file mirrors it for JSON).
#
# Every record is open (`..`): consumers that read fields by name (the
# gitbot-fleet scripts, .hypatia-baseline.json matching) are unaffected by
# the verdict fields, and later additive fields do not break this contract.
#
# `level` is not free text.  `LevelDerived` re-derives it from `support` and
# `evidence` and refuses a record whose stored level disagrees:
#   sound  — support is `mechanical` and its re-check `confirms`;
#   epi    — any other support, with at least one evidence token;
#   belief — no evidence token at all (a Bayesian posterior alone).
# A pattern match therefore can never be reported "sound".
#
# Check a file:  scripts/check-findings-schema.sh <findings.json>

let one_of = fun allowed =>
  std.contract.from_predicate (fun value => std.array.elem value allowed)
in

let severity_c = one_of ["critical", "high", "medium", "low", "info"] in

let level_c = one_of ["sound", "epi", "belief"] in

let sha256_c =
  std.contract.from_predicate (fun s =>
    std.is_string s && std.string.is_match "^[0-9a-f]{64}$" s)
in

let nat_c =
  std.contract.from_predicate (fun n =>
    std.is_number n && std.number.is_integer n && n >= 0)
in

# Some rule modules emit `"line": null` for file- or repo-level findings.
let line_c =
  std.contract.from_predicate (fun n =>
    n == null || (std.is_number n && std.number.is_integer n && n >= 0))
in

# What a verdict's evidence supports (Verdict.idr `Support`).
#   pattern    — a pattern matched: evidence that the pattern matched, and
#                nothing more.
#   observed   — the finding has been observed `observations` times across
#                the fleet.  rules.a2ml proposes a rule at 5 observations and
#                auto-approves at 10 + 3 successful fixes; those thresholds
#                are BELIEF-FORMATION rules, not soundness proofs, and no
#                count makes a finding sound.
#   mechanical — a prover re-checked the flagged file; both digests are
#                recorded so the re-check can be repeated and compared.
let support_c =
  std.contract.custom (fun label value =>
    if !(std.is_record value) || !(std.record.has_field "kind" value) then
      'Error { message = "support must be a record with a `kind` field" }
    else if value.kind == "pattern" then
      'Ok value
    else if value.kind == "observed" then
      'Ok (value | { kind | String, observations | nat_c, .. })
    else if value.kind == "mechanical" then
      'Ok (
        value
        | {
          kind | String,
          prover | one_of ["lean4", "idris2", "agda", "coq"],
          prover_version | String,
          target | String,
          target_sha256 | sha256_c,
          artefact_sha256 | sha256_c,
          result | one_of ["confirms", "refutes"],
          ..
        }
      )
    else
      'Error { message = "support.kind must be pattern, observed or mechanical" })
in

# One piece of evidence a rule offered (Verdict.idr `EvidenceToken`).
let evidence_token_c = {
  rule | String,
  rule_version | String,
  span | { file | String, start_line | nat_c, end_line | nat_c, .. },
  observations | nat_c,
  ..
}
in

let derive_level_fn = fun support evidence =>
  if support.kind == "mechanical" && support.result == "confirms" then
    "sound"
  else if std.array.length evidence > 0 then
    "epi"
  else
    "belief"
in

let verdict_fields_c = {
  support | support_c,
  evidence | Array evidence_token_c,
  level | level_c,
  ..
}
in

# `LevelDerived`: the stored level must be the derived one.
let level_derived_c =
  std.contract.custom (fun label value =>
    let expected = derive_level_fn value.support value.evidence in
    if value.level == expected then
      'Ok value
    else
      'Error {
        message = "level is \"%{value.level}\" but support and evidence derive \"%{expected}\"",
      })
in

# A finding as the escript emits it.
let escript_fields_c = {
  rule_module | String,
  type | String,
  severity | severity_c,
  file | String,
  line | line_c | optional,
  reason | String,
  action | String,
  ..
}
in

# A finding as the bash fallback emits it.
let bash_fields_c = {
  severity | severity_c,
  type | String,
  pattern | String,
  file | String,
  line | nat_c,
  code | String,
  cwe | String,
  fix | String,
  auto_fixable | Bool,
  ..
}
in

let finding_c = std.contract.Sequence [escript_fields_c, verdict_fields_c, level_derived_c] in

let bash_finding_c = std.contract.Sequence [bash_fields_c, verdict_fields_c, level_derived_c] in

{
  Severity = severity_c,
  Level = level_c,
  Support = support_c,
  EvidenceToken = evidence_token_c,
  Finding = finding_c,
  BashFinding = bash_finding_c,
  derive_level = derive_level_fn,

  # `hypatia-cli.sh scan --format json` (escript).
  Findings = Array finding_c,

  # `hypatia-cli-bash.sh scan --format json`.
  BashReport = {
    submission_metadata | {
      repo | String,
      timestamp | String,
      scanner | String,
      version | String,
      finding_count | nat_c,
      ..
    },
    findings | Array bash_finding_c,
    ..
  },
}
