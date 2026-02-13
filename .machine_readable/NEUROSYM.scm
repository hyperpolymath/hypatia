;; SPDX-License-Identifier: PMPL-1.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for cicd-hyper-a
;; Media-Type: application/vnd.neurosym+scm

(neurosym
  (version "1.0")
  (project "cicd-hyper-a")

  (architecture
    (model "neural-learning-symbolic-execution")
    (description "5 neural networks detect patterns and estimate confidence; Logtalk rules execute fixes deterministically")

    (layers
      (neural-layer
        (purpose "Multi-network intelligence for pattern analysis and dispatch")
        (networks
          (graph-of-trust
            (type "PageRank-style")
            (implementation "lib/neural/graph_of_trust.ex")
            (purpose "Trust-weighted dispatch routing over repos/bots/recipes")
            (parameters (damping-factor 0.85) (convergence-threshold 0.001) (max-iterations 50) (decay-rate 0.95)))
          (mixture-of-experts
            (type "Sparse MoE with gating network")
            (implementation "lib/neural/mixture_of_experts.ex")
            (purpose "Domain-specific confidence estimation")
            (experts "security" "quality" "sustainability" "accessibility" "performance" "documentation" "infrastructure")
            (parameters (top-k 2) (learning-rate 0.01) (load-balance-weight 0.01)))
          (liquid-state-machine
            (type "Reservoir computing")
            (implementation "lib/neural/liquid_state_machine.ex")
            (purpose "Temporal anomaly detection in event streams")
            (parameters (reservoir-size 100) (spectral-radius 0.9) (leak-rate 0.3) (sparsity 0.1)))
          (echo-state-network
            (type "Reservoir computing / time-series")
            (implementation "lib/neural/echo_state_network.ex")
            (purpose "Confidence trajectory forecasting and drift detection")
            (parameters (reservoir-size 80) (spectral-radius 0.95) (leak-rate 0.2) (washout 50)))
          (radial-neural-network
            (type "Radial Basis Function network")
            (implementation "lib/neural/radial_neural_network.ex")
            (purpose "Finding similarity, novelty detection, pattern classification")
            (parameters (num-centers 20) (kmeans-iterations 50) (width-scale 1.5))))
        (coordinator
          (implementation "lib/neural/coordinator.ex")
          (purpose "Orchestrates all 5 networks as OTP GenServer")
          (aggregation "MoE 60% + RBF 25% + LSM 15%"))
        (inputs
          ("verisimdb-data scan results")
          ("Outcome records from gitbot-fleet")
          ("Temporal event streams (scans, dispatches, outcomes)")
          ("Finding feature vectors"))
        (outputs
          ("Aggregated confidence scores")
          ("Dispatch strategy recommendations")
          ("Novelty flags for unknown findings")
          ("Anomaly alerts from event streams")
          ("Confidence trajectory forecasts")))

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
