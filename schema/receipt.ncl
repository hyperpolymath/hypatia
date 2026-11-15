# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Hypatia E0 receipt schema: the contract of one receipt line
# (receipts/YYYY-MM.jsonl). The design is docs/design/e0-receipt.adoc
# (RULED 2026-11-15); this file transcribes its field table and its rules.
#
# A receipt says: this event changed what a learner holds, and here is the exact
# evidence that it rested on. It names a row by id and never a count. The
# contract refuses a receipt that claims more than its evidence:
#
#   * `verdict` is DERIVED. It must be the answer of the single gate
#     (OutcomeTracker.qualified_outcome_counts/1) for the claimed outcome and the
#     qualification, so a receipt cannot call an ignored row a credited one.
#     `withdrawn` is the one other value and needs a parent receipt.
#   * `qualification` "verified" needs an evidence reference and observations.
#   * `missing_evidence` is EXACT: a field that is absent is named, and a field
#     that is named is absent. An empty list does not mean "complete", it means
#     nothing is known to be missing.
#   * No JSON number occurs anywhere (the profile of scripts/proof-ledger.sh, in
#     which `jq -cS` is the canonical form): counts and hit numbers are decimal
#     strings.
#
# Check a file:  scripts/check-receipt-schema.sh <receipt.json>
# Self-test:     scripts/check-receipt-schema.sh --self-test

let one_of = fun allowed =>
  std.contract.from_predicate (fun value => std.array.elem value allowed)
in

let sha256_c =
  std.contract.from_predicate (fun s => std.is_string s && std.string.is_match "^[0-9a-f]{64}$" s)
in

# ADR-008: version 8, variant 10, lowercase 8-4-4-4-12.
let event_id_c =
  std.contract.from_predicate (fun s =>
    std.is_string s
    && std.string.is_match "^[0-9a-f]{8}-[0-9a-f]{4}-8[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$" s)
in

let decimal_c =
  std.contract.from_predicate (fun s => std.is_string s && std.string.is_match "^[0-9]+$" s)
in

# A path relative to the data root, with no way out of it.
let rel_path_c =
  std.contract.from_predicate (fun s =>
    std.is_string s
    && std.string.is_match "^[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)*$" s
    && !(std.string.contains ".." s))
in

let ascii_c =
  std.contract.from_predicate (fun s => std.is_string s && std.string.is_match "^[ -~]*$" s)
in

let qualification_c =
  one_of ["verified", "failed", "incomplete", "still_present", "rollback_requested", "unqualified"]
in

let verdict_c =
  one_of ["credited_success", "credited_failure", "credited_false_positive", "audit_only", "withdrawn"]
in

let claimed_outcome_c = one_of ["success", "failure", "false_positive", "unknown"] in

let source_kind_c = one_of ["fleet_import", "local_rescan", "local_caller", "neural_path"] in

let event_id_source_c = one_of ["producer", "hypatia"] in

let rescan_c = one_of ["verified", "still_present", "scan_failed", "scan_skipped"] in

# The closed set of names that missing_evidence can hold.
let missing_names = [
  "after_hits", "before_hits", "event_id", "exit_code", "head_sha", "match_kind", "pattern_id",
  "rescan", "scan_digest", "subject_id", "subject_identity", "test_digest", "tool_version",
  "verdict",
] in

let sorted_unique = fun arr =>
  let cmp = fun a b =>
    if a == b then 'Equal else if (std.string.compare a b) == 'Lesser then 'Lesser else 'Greater
  in
  let seen = std.array.fold_left (fun acc x => if std.array.elem x acc then acc else acc @ [x]) [] arr in
  (std.array.sort cmp arr) == arr && (std.array.length seen) == (std.array.length arr)
in

let sorted_unique_c = std.contract.from_predicate (fun arr => std.is_array arr && sorted_unique arr) in

let missing_evidence_c =
  std.contract.from_predicate (fun arr =>
    std.is_array arr
    && sorted_unique arr
    && std.array.all (fun x => std.array.elem x missing_names) arr)
in

let parent_ids_c =
  std.contract.from_predicate (fun arr =>
    std.is_array arr
    && sorted_unique arr
    && std.array.all (fun x => std.is_string x && std.string.is_match "^[0-9a-f]{8}-[0-9a-f]{4}-8[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$" x) arr)
in

# What the gate does with a row: the same four-way answer as
# OutcomeTracker.qualified_outcome_counts/1 (one row, one bucket).
let verdict_for = fun qualification claimed =>
  if claimed == "success" && qualification == "verified" then
    "credited_success"
  else if claimed == "failure" && std.array.elem qualification ["failed", "incomplete", "verified"] then
    "credited_failure"
  else if claimed == "false_positive"
    && std.array.elem qualification ["still_present", "verified", "rollback_requested"] then
    "credited_false_positive"
  else
    "audit_only"
in

# A field that may be absent: present exactly when it is not in missing_evidence.
let gap_exact = fun name present missing => (present == !(std.array.elem name missing)) in

let receipt_fields_c = {
  assertion
    | { claimed_outcome | claimed_outcome_c, file | ascii_c, recipe_id | ascii_c, repo | ascii_c },
  chain_version | std.contract.Equal "e0-receipt/1",
  event_id | event_id_c | optional,
  head_sha | std.contract.from_predicate (fun s => std.is_string s && std.string.is_match "^[0-9a-f]{40}$" s) | optional,
  match_kind | one_of ["exact", "category", "fuzzy"] | optional,
  missing_evidence | missing_evidence_c,
  observations
    | {
      after_hits | decimal_c | optional,
      before_hits | decimal_c | optional,
      exit_code | decimal_c | optional,
      rescan | rescan_c | optional,
      verdict | ascii_c | optional,
    },
  parent_event_ids | parent_ids_c,
  pattern_id | ascii_c | optional,
  proof_or_test_ref
    | {
      row_sha256 | sha256_c,
      scan_digest | sha256_c | optional,
      test_digest | sha256_c | optional,
    },
  provenance
    | { event_id_source | event_id_source_c, producer | ascii_c, tool_version | ascii_c | optional, writer | std.contract.Equal "hypatia" },
  qualification | qualification_c,
  receipt_sha256 | sha256_c,
  source | { kind | source_kind_c, line | decimal_c, log | rel_path_c },
  subject_id | event_id_c | optional,
  verdict | verdict_c,
}
in

let receipt_rules_c = std.contract.custom (fun label r =>
    let m = r.missing_evidence in
    let has = fun f obj => std.record.has_field f obj in
    # verdict is derived, or a withdrawal with a parent
    let verdict_ok =
      if r.verdict == "withdrawn" then
        (std.array.length r.parent_event_ids) > 0
      else
        r.verdict == (verdict_for r.qualification r.assertion.claimed_outcome)
    in
    # verified needs evidence and observations
    let verified_ok =
      r.qualification != "verified" || (r.observations != {} && has "row_sha256" r.proof_or_test_ref)
    in
    # missing_evidence is exact
    let gaps_ok =
      (gap_exact "event_id" (has "event_id" r) m)
      && (gap_exact "head_sha" (has "head_sha" r) m)
      && (gap_exact "subject_id" (has "subject_id" r) m)
      && (gap_exact "pattern_id" (has "pattern_id" r) m)
      && (gap_exact "match_kind" (has "match_kind" r) m)
      && (gap_exact "after_hits" (has "after_hits" r.observations) m)
      && (gap_exact "before_hits" (has "before_hits" r.observations) m)
      && (gap_exact "exit_code" (has "exit_code" r.observations) m)
      && (gap_exact "rescan" (has "rescan" r.observations) m)
      && (gap_exact "verdict" (has "verdict" r.observations) m)
      && (gap_exact "scan_digest" (has "scan_digest" r.proof_or_test_ref) m)
      && (gap_exact "test_digest" (has "test_digest" r.proof_or_test_ref) m)
      && (gap_exact "tool_version" (has "tool_version" r.provenance) m)
      # no stable finding identity exists yet (issue 984, E1)
      && (std.array.elem "subject_identity" m)
    in
    if !verdict_ok then
      'Error { message = "verdict is not the answer of the gate for this qualification and claimed outcome (and is not a withdrawal with a parent)" }
    else if !verified_ok then
      'Error { message = "qualification verified needs observations and an evidence reference" }
    else if !gaps_ok then
      'Error { message = "missing_evidence is not exact: a field that is absent is not named, or a field that is named is present" }
    else
      'Ok r)
in

let receipt_c = std.contract.Sequence [receipt_fields_c, receipt_rules_c] in

{ Receipt = receipt_c }
