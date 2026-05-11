K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# K9 Kennel-level guard: hypatia project metadata and CI/CD invariants
# Security Level: Kennel (data-only, no execution)
# No signature required

{
  pedigree = {
    name = "hypatia-metadata",
    leash = 'Kennel,
    schema_version = "1.0.0",
    component_type = "ci-tool-metadata",
    security = {
      leash = 'Kennel,
      trust_level = "data-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      name = "hypatia-metadata",
      version = "0.1.0",
      description = "Hypatia: neurosymbolic CI/CD intelligence platform — project invariants",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  # Project configuration
  config = {
    # Workspace crates that must be present
    required_crates = [
      "adapters",
      "cli",
      "data",
      "fixer",
      "integration",
    ],

    # Rule module requirements
    rules = {
      # Minimum rule modules in the platform
      min_rule_modules = 15,
      # Each rule module must have tests
      tests_per_module_required = true,
    },

    # Safety invariants
    safety = {
      forbidden_patterns = [
        "unwrap()",
        "expect()",
        "todo!()",
        "unimplemented!()",
      ],
    },

    # Test requirements
    tests = {
      min_total_tests = 500,
      integration_tests_required = true,
    },

    # Build configuration
    build = {
      edition = "2021",
      msrv = "1.75",
      workspace_resolver = "2",
    },

    # Language policy
    banned_files = ["Makefile", "Dockerfile"],
    banned_languages = ["python", "typescript", "javascript", "go"],

    # CI/CD integration invariants
    ci = {
      # Must scan all RSR repos
      target_repo_count_min = 200,
      # Dispatch to gitbot-fleet must be JSONL-logged
      dispatch_logging_required = true,
    },
  },

  # RSR compliance
  rsr = {
    explainme = true,
    ai_manifest = true,
    machine_readable = true,
    spdx_headers_required = true,
    security_md_required = true,
    idris2_abi = true,
    zig_ffi = true,
  },
}
