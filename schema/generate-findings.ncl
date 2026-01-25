# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia Findings Generator
# Generates type-safe JSON from scanner patterns

let schema = import "findings.ncl" in

# This will be populated by the bash scanner
# Format: array of finding objects
let findings_data | Array schema.Finding = std.deserialize 'Json in

# Scan metadata (will be provided via environment)
let scan_info = schema.make_scan_info
  (std.string.from_enum (std.env.HYPATIA_TIMESTAMP))
  (std.string.from_enum (std.env.HYPATIA_REPOSITORY))
  "0.1.0"  # Scanner version
  (std.string.to_number (std.string.from_enum (std.env.HYPATIA_FILES_SCANNED)))
  (std.array.length findings_data)
in

# Generate complete report
schema.make_report scan_info findings_data
