K9!
# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia Fleet Configuration
#
# K9 Contractile defining hypatia's fleet-wide scanning configuration,
# rule thresholds, learning loop parameters, and integration endpoints.

leash = 'Hunt

pedigree = {
  schema_version = "1.0.0",
  component_type = "security-scanner-fleet-config",
  author = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>",
  description = "Hypatia neurosymbolic security scanner fleet configuration",
  created = "2026-02-06",
  updated = "2026-02-06",
  k9_spec_version = "1.0.0",
}

# Fleet-wide scanner configuration
config = {
  # Scanner metadata
  scanner = {
    name | String = "hypatia",
    version | String = "1.0.0",
    type | String = "neurosymbolic-security-scanner",
    description | String = "Pattern-learning security scanner with autonomous rule generation",
  },

  # Supervised repositories (high-priority, actively monitored)
  supervised_repos | {
    core | Array String,
    formal | Array String,
    active | Array String,
  } = {
    core = [
      "echidna",
      "echidnabot",
      "hypatia",
      "gitbot-fleet",
      "robot-repo-automaton",
      "glambot",
      "seambot",
      "finishingbot",
      "accessibilitybot",
      "cipherbot",
    ],
    formal = [
      "absolute-zero",
      "proven",
      "affinescript",
      "cerro-torre",
    ],
    active = [
      "academic-workflow-suite",
      "vordr",
      "lithoglyph",
      "scaffoldia",
      "svalinn",
    ],
  },

  # Scan patterns (detection rules)
  scan_patterns = {
    # Pattern 1: ReScript getExn (CRITICAL)
    unsafe_crash = {
      enabled | Bool = true,
      severity | String = "critical",
      cwe | String = "CWE-754",
      languages | Array String = ["rescript"],
      auto_fixable | Bool = false,
      observations | Number = 342,
      status | String = "approved",
    },

    # Pattern 2: Rust unwrap (HIGH)
    unsafe_panic = {
      enabled | Bool = true,
      severity | String = "high",
      cwe | String = "CWE-754",
      languages | Array String = ["rust"],
      auto_fixable | Bool = false,
      observations | Number = 1150,
      status | String = "approved",
    },

    # Pattern 3: Obj.magic (HIGH)
    type_safety_bypass = {
      enabled | Bool = true,
      severity | String = "high",
      cwe | String = "CWE-704",
      languages | Array String = ["rescript"],
      auto_fixable | Bool = false,
      observations | Number = 477,
      status | String = "approved",
    },

    # Pattern 4: CORS wildcard (CRITICAL)
    cors_misconfiguration = {
      enabled | Bool = true,
      severity | String = "critical",
      cwe | String = "CWE-942",
      languages | Array String = ["rust", "rescript", "javascript"],
      auto_fixable | Bool = true,
      auto_fix_script | String = "scripts/fix-cors-wildcard.sh",
      observations | Number = 3,
      fixes_executed | Number = 3,
      status | String = "approved",
    },

    # Pattern 5: Hardcoded secrets (CRITICAL)
    hardcoded_secret = {
      enabled | Bool = true,
      severity | String = "critical",
      cwe | String = "CWE-798",
      languages | Array String = ["all"],
      auto_fixable | Bool = false,
      observations | Number = 0,
      status | String = "active",
    },

    # Pattern 6: eval() usage (CRITICAL)
    eval_usage = {
      enabled | Bool = true,
      severity | String = "critical",
      cwe | String = "CWE-95",
      languages | Array String = ["javascript", "rescript"],
      auto_fixable | Bool = false,
      observations | Number = 7,
      status | String = "approved",
    },

    # Pattern 7: Undocumented unsafe blocks (HIGH)
    unsafe_without_doc = {
      enabled | Bool = true,
      severity | String = "high",
      cwe | String = "CWE-1188",
      languages | Array String = ["rust"],
      auto_fixable | Bool = false,
      observations | Number = 19,
      status | String = "approved",
    },

    # Pattern 8: Technical debt markers (INFO)
    technical_debt = {
      enabled | Bool = true,
      severity | String = "info",
      cwe | String = "CWE-1057",
      languages | Array String = ["all"],
      auto_fixable | Bool = false,
      observations | Number = 196,
      status | String = "approved",
      priority_markers | {
        bug | String,
        fixme | String,
        hack | String,
        todo | String,
        xxx | String,
      } = {
        bug = "critical",
        fixme = "high",
        hack = "medium",
        todo = "low",
        xxx = "low",
      },
    },
  },

  # Learning loop parameters
  learning_loop = {
    # Rule proposal thresholds
    rule_proposal_threshold | Number = 5,
    auto_approval_threshold | {
      observations | Number,
      successful_fixes | Number,
    } = {
      observations = 10,
      successful_fixes = 3,
    },

    # Validation requirements
    validation = {
      required_score | Number = 0.90,  # 90% validation success rate
      validator | String = "echidna-semantic-analysis",
      checks | Array String = [
        "structural_validation",
        "predicate_completeness",
        "logical_consistency",
        "pattern_coverage",
        "rule_interactions",
        "completeness",
        "quality_metrics",
      ],
    },

    # Observation storage
    storage = {
      observations_file | String = "observed-patterns.jsonl",
      auto_fix_history_file | String = "auto-fix-history.jsonl",
      rule_proposals_dir | String = "rule-proposals",
      approved_rules_dir | String = "approved-rules",
    },
  },

  # Integration endpoints
  integration = {
    # Fleet coordinator
    fleet_coordinator = {
      url | String = "tcp://localhost:5000",
      shared_context_path | String = "/var/mnt/eclipse/repos/gitbot-fleet/shared-context",
      findings_dir | String = "findings",
      learning_dir | String = "learning",
    },

    # ECHIDNA validation
    echidna = {
      url | String = "http://localhost:8080",
      api_version | String = "v1",
      validation_endpoint | String = "/validate",
      proof_endpoint | String = "/prove",
    },

    # Robot-repo-automaton (auto-fix execution)
    automaton = {
      url | String = "tcp://localhost:5001",
      auto_fix_dir | String = "/var/mnt/eclipse/repos/gitbot-fleet/shared-context/auto-fixes",
      execution_mode | String = "supervised",  # supervised | autonomous
    },

    # GitHub integration
    github = {
      org | String = "hyperpolymath",
      api_url | String = "https://api.github.com",
      graphql_url | String = "https://api.github.com/graphql",
    },
  },

  # Scan scheduling
  scan_schedule = {
    # Supervised repos: daily scans
    supervised | {
      cron | String,
      on_push | Bool,
      on_pull_request | Bool,
    } = {
      cron = "0 0 * * *",  # Daily at midnight UTC
      on_push = true,
      on_pull_request = true,
    },

    # General group: weekly scans
    general | {
      cron | String,
      on_push | Bool,
      on_pull_request | Bool,
    } = {
      cron = "0 0 * * 0",  # Weekly on Sunday
      on_push = false,
      on_pull_request = true,
    },
  },

  # Output formats
  output = {
    default_format | String = "json",
    submission_envelope | Bool = true,
    include_metadata | Bool = true,

    formats | {
      json | Bool,
      github_annotations | Bool,
      text | Bool,
    } = {
      json = true,
      github_annotations = true,
      text = false,
    },
  },

  # Performance tuning
  performance = {
    max_parallel_scans | Number = 4,
    scan_timeout_seconds | Number = 300,
    max_findings_per_repo | Number = 1000,
    file_size_limit_mb | Number = 10,
  },

  # Severity filters
  severity_filters = {
    default_threshold | String = "medium",  # info | low | medium | high | critical

    by_context | {
      production | String,
      test | String,
      development | String,
    } = {
      production = "high",
      test = "medium",
      development = "medium",
    },
  },

  # Auto-fix policy
  auto_fix = {
    enabled | Bool = false,  # Global kill switch
    require_approval | Bool = true,

    allowed_patterns | Array String = [
      "cors_misconfiguration",  # Only approved auto-fixable pattern
    ],

    safety_checks | {
      dry_run_first | Bool,
      require_tests_passing | Bool,
      max_fixes_per_run | Number,
    } = {
      dry_run_first = true,
      require_tests_passing = true,
      max_fixes_per_run = 10,
    },
  },

  # Monitoring and alerting
  monitoring = {
    enabled | Bool = true,

    metrics | {
      observations_per_pattern | Bool,
      false_positive_rate | Bool,
      fix_success_rate | Bool,
      scan_duration | Bool,
    } = {
      observations_per_pattern = true,
      false_positive_rate = true,
      fix_success_rate = true,
      scan_duration = true,
    },

    alerts | {
      false_positive_rate_threshold | Number,
      scan_failure_threshold | Number,
      notification_channels | Array String,
    } = {
      false_positive_rate_threshold = 0.20,  # Alert if >20% false positives
      scan_failure_threshold = 3,  # Alert after 3 consecutive failures
      notification_channels = ["github-issues"],
    },
  },

  # Feature flags
  features = {
    neurosymbolic_learning | Bool = true,
    autonomous_rule_generation | Bool = true,
    auto_fix_execution | Bool = false,  # Disabled pending more testing
    echidna_validation | Bool = true,
    fleet_coordination | Bool = true,
  },
}

# Deployment manifest
manifest = {
  deployed_at | String = "2026-02-06T22:45:00+00:00",
  deployed_by | String = "gitbot-fleet-coordinator",
  deployment_id | String = "DEPLOY-20260206-001",

  active_rules | Number = 7,
  total_observations | Number = 2194,
  fleet_coverage_percent | Number = 2.4,

  status | String = "production-ready",
  monitoring_status | String = "active",

  next_review_date | String = "2026-02-13",
}
