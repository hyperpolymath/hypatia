# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia Security Findings Schema
# Type-safe schema for scanner output

let Severity = [| 'critical, 'high, 'medium, 'low |] in
let FindingType = [|
  'unsafe_crash,
  'unsafe_panic,
  'type_safety_bypass,
  'cors_misconfiguration,
  'auth_bypass,
  'unverified_data,
|] in

let Finding = {
  severity | Severity,
  type | FindingType,
  pattern | String,
  file | String,
  line | Number,
  code | String,
  cwe | String,
  fix | String,
} in

let ScanInfo = {
  timestamp | String,
  repository | String,
  scanner_version | String,
  scanned_files | Number,
  total_findings | Number,
} in

let FindingsReport = {
  scan_info | ScanInfo,
  findings | Array Finding,
} in

{
  # Schema exports
  Severity,
  FindingType,
  Finding,
  ScanInfo,
  FindingsReport,

  # Helper function to create a finding
  make_finding = fun severity type pattern file line code cwe fix =>
    {
      severity = severity,
      type = type,
      pattern = pattern,
      file = file,
      line = line,
      code = code,
      cwe = cwe,
      fix = fix,
    } | Finding,

  # Helper to create scan info
  make_scan_info = fun timestamp repository version files count =>
    {
      timestamp = timestamp,
      repository = repository,
      scanner_version = version,
      scanned_files = files,
      total_findings = count,
    } | ScanInfo,

  # Helper to create complete report
  make_report = fun scan_info findings =>
    {
      scan_info = scan_info,
      findings = findings,
    } | FindingsReport,
}
