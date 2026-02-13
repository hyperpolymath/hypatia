;; SPDX-License-Identifier: PMPL-1.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for cicd-hyper-a
;; Media-Type: application/vnd.neurosym+scm

(neurosym
  (version "1.0")
  (project "cicd-hyper-a")

  (architecture
    (model "neural-learning-symbolic-execution")
    (description "Neural models detect patterns from CI/CD failures, distilled into Logtalk rules for fast, reliable execution")

    (layers
      (neural-layer
        (purpose "Pattern detection from CI/CD failure data")
        (models
          (slm "Local SLM (Qwen3-4B or similar) for privacy-preserving analysis")
          (embedding "Sentence transformers for error similarity"))
        (inputs
          ("GitHub Actions run logs")
          ("Dependabot alerts")
          ("CodeQL scan results")
          ("OpenSSF Scorecard reports"))
        (outputs
          ("Confidence-scored patterns")
          ("Error cluster assignments")
          ("Fix suggestions")))

      (distillation-layer
        (purpose "Convert neural patterns to symbolic rules")
        (implementation "rule_distiller.lgt")
        (threshold 0.85 "Minimum confidence to distill")
        (validation
          ("Check for rule conflicts")
          ("Verify against existing catalog")
          ("Human review for critical rules")))

      (symbolic-layer
        (purpose "Fast, deterministic rule execution")
        (implementation "cicd_rules.lgt, security_errors.lgt")
        (features
          ("Severity classification")
          ("Auto-fix pattern matching")
          ("Dependency analysis")
          ("Preventive blocking")))))

  (benefits
    (dependability
      (principle "Symbolic rules are deterministic and auditable")
      (practice "All auto-fixes use verified Logtalk predicates")
      (outcome "Consistent behavior across 200+ repos"))

    (security
      (principle "Neural layer never directly executes - only suggests")
      (practice "Distilled rules require confidence threshold")
      (outcome "No hallucination-based actions"))

    (performance
      (principle "Symbolic execution is O(1) after distillation")
      (practice "Logtalk rules compiled to native predicates")
      (outcome "Sub-second fix application across repos"))

    (usability
      (principle "Users interact with symbolic layer, not neural")
      (practice "Clear error IDs (ERR-WF-001) and fix suggestions")
      (outcome "Human-readable remediation guidance"))

    (functionality
      (principle "Neural layer enables continuous improvement")
      (practice "New patterns automatically proposed for distillation")
      (outcome "Self-improving error catalog")))

  (integration-points
    (virtuoso-db
      (purpose "RDF/SPARQL storage for learned patterns")
      (status "planned")
      (benefit "Query patterns across time, cross-repo correlation"))

    (repo-slm-augmentor
      (purpose "SLM hosting and inference")
      (status "active")
      (benefit "Local inference without cloud API costs"))

    (gitvisor
      (purpose "Real-time monitoring data feed")
      (status "active")
      (benefit "Fresh failure data for pattern detection")))

  (safeguards
    (human-in-loop "Critical severity fixes require approval")
    (rollback "All changes versioned, instant rollback possible")
    (audit-trail "Every distilled rule tracked with provenance")
    (confidence-gates "Low confidence patterns flagged for review")))
