;; SPDX-License-Identifier: PMPL-1.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for cicd-hyper-a
;; Media-Type: application/vnd.agentic+scm

(agentic
  (version "1.0")
  (project "cicd-hyper-a")

  (agent-roles
    (scanner-agent
      (purpose "Continuous scanning of repos for issues")
      (autonomy "high")
      (actions
        ("Scan workflow files for errors")
        ("Check SHA pinning status")
        ("Verify permissions declarations")
        ("Detect language mismatches"))
      (triggers
        ("Scheduled cron (hourly)")
        ("Push to main branch")
        ("New repo creation"))
      (outputs
        ("Issue reports to error catalog")
        ("Metrics to gitvisor")))

    (fixer-agent
      (purpose "Apply auto-fixes to detected issues")
      (autonomy "medium")
      (actions
        ("Update SHA pins to latest valid")
        ("Add missing permissions declarations")
        ("Insert SPDX headers")
        ("Fix CodeQL language matrices"))
      (triggers
        ("Scanner agent finds auto-fixable issue")
        ("Manual invocation"))
      (outputs
        ("Pull requests with fixes")
        ("Commit history updates"))
      (constraints
        ("Requires approval for critical severity")
        ("Rate limited to prevent spam")))

    (propagator-agent
      (purpose "Wave propagation of fixes across repos")
      (autonomy "low")
      (actions
        ("Generate git hooks from rules")
        ("Push to .git-private-farm")
        ("Trigger hook installation across repos"))
      (triggers
        ("New rule added to catalog")
        ("Manual deployment request"))
      (outputs
        ("Updated hooks in target repos")
        ("Deployment receipts"))
      (constraints
        ("Requires explicit approval")
        ("Staged rollout: test repos first")))

    (learning-agent
      (purpose "Pattern detection from failure data")
      (autonomy "high")
      (actions
        ("Analyze failure logs")
        ("Cluster similar errors")
        ("Propose new patterns for distillation"))
      (triggers
        ("New failure data available")
        ("Weekly batch analysis"))
      (outputs
        ("Pattern proposals to distillation layer")
        ("Trend reports"))
      (constraints
        ("Never directly executes fixes")
        ("Patterns require human review for critical"))))

  (interaction-patterns
    (human-ai-collaboration
      (pattern "Approval gates for sensitive actions")
      (implementation "AskUserQuestion tool integration")
      (use-cases
        ("Critical severity fix approval")
        ("New rule distillation review")
        ("Cross-repo propagation confirmation")))

    (agent-to-agent
      (pattern "Event-driven communication")
      (implementation "Message passing via gitvisor")
      (use-cases
        ("Scanner notifies Fixer of issue")
        ("Fixer requests Propagator deployment")
        ("Learning shares patterns with Scanner")))

    (continuous-improvement
      (pattern "Feedback loop for rule refinement")
      (implementation "Track fix success/failure rates")
      (use-cases
        ("Adjust confidence thresholds")
        ("Retire ineffective rules")
        ("Promote high-success patterns"))))

  (safety-mechanisms
    (autonomy-levels
      (high "Can act without approval on low-severity issues")
      (medium "Requires approval for destructive or cross-repo actions")
      (low "All actions require explicit human approval"))

    (rollback-capability
      (principle "Every agent action is reversible")
      (implementation "Git-based versioning, instant revert"))

    (rate-limiting
      (purpose "Prevent runaway automation")
      (implementation "Max 10 PRs per hour per agent"))

    (audit-logging
      (purpose "Full traceability of agent actions")
      (implementation "Structured logs to system-observatory")))

  (enhancement-opportunities
    (multi-agent-orchestration
      (description "Coordinate multiple agents for complex remediation")
      (benefit "Handle multi-step fixes atomically")
      (status "planned"))

    (predictive-prevention
      (description "Predict issues before they manifest")
      (benefit "Proactive rather than reactive fixes")
      (status "research"))

    (cross-account-federation
      (description "Share learned patterns across organizations")
      (benefit "Community-wide immunity")
      (status "future"))))
