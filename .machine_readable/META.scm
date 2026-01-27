;; SPDX-License-Identifier: MPL-2.0-or-later
;; META.scm - Meta-level information for cicd-hyper-a
;; Media-Type: application/meta+scheme

(meta
  (architecture-decisions
    (adr-001
      (status "accepted")
      (date "2025-12-29")
      (title "Use Logtalk for rule engine")
      (context "Need a symbolic reasoning system that can encode CI/CD rules learned from neural analysis of 228 repos and 1922 alerts")
      (decision "Use Logtalk with objects for modular rule organization")
      (consequences
        ("Pro: Clean separation of rule categories")
        ("Pro: Can integrate with Prolog backends like SWI-Prolog")
        ("Pro: Object-oriented encapsulation of rule sets")
        ("Con: Smaller community than pure Prolog")))

    (adr-002
      (status "accepted")
      (date "2025-12-29")
      (title "Neurosymbolic architecture - neural learning, symbolic execution")
      (context "Need to learn patterns from CI/CD failures but execute fixes reliably")
      (decision "Use neural models (SLM) for pattern detection, distill to Logtalk rules for execution")
      (consequences
        ("Pro: Fast symbolic execution after learning")
        ("Pro: Explainable rules can be audited")
        ("Pro: Rules can be manually tuned")
        ("Con: Distillation process needs refinement")))

    (adr-003
      (status "accepted")
      (date "2026-01-04")
      (title "Hook wave propagation via .git-private-farm")
      (context "Need to deploy fixes across 200+ repos without manual intervention")
      (decision "Use .git-private-farm as central hook distribution point, cicd-hyper-a generates hooks")
      (consequences
        ("Pro: Single source of truth for hooks")
        ("Pro: Can version control hook changes")
        ("Pro: Atomic updates across repos")
        ("Con: Requires git-private-farm infrastructure")))

    (adr-004
      (status "accepted")
      (date "2026-01-04")
      (title "Error catalog in Logtalk with auto-fix patterns")
      (context "Need machine-readable error definitions with fix patterns")
      (decision "Encode errors as Logtalk facts: error(ID, Category, Severity, Description, AutoFixable)")
      (consequences
        ("Pro: Queryable error database")
        ("Pro: Can generate fixes programmatically")
        ("Pro: Severity-based prioritization")
        ("Con: Manual maintenance of error catalog"))))

  (development-practices
    (code-style
      (logtalk "Follow Logtalk coding guidelines")
      (rust "rustfmt, clippy")
      (julia "JuliaFormatter"))
    (security
      (principle "Defense in depth")
      (sha-pinning "All GitHub Actions must be SHA-pinned")
      (permissions "Workflows use permissions: read-all by default")
      (spdx "All files have SPDX-License-Identifier header")
      (secrets "Never hardcode - use environment variables"))
    (testing
      (logtalk "Logtalk unit tests via lgtunit")
      (integration "GitHub Actions workflow tests"))
    (versioning "SemVer")
    (documentation "AsciiDoc primary, Markdown for GitHub compatibility")
    (branching "main for stable, feature/* for development"))

  (design-rationale
    (why-logtalk "Provides object-oriented layer over Prolog for modular rule organization")
    (why-julia-for-batch "RSR-compliant scripting language for batch operations")
    (why-neurosymbolic "Combines learning capability of neural with reliability of symbolic")
    (why-hook-propagation "Enables atomic cross-repo updates without CI/CD pipeline changes")
    (why-error-catalog "Machine-readable errors enable automated remediation")))
