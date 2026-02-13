;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for hypatia
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "2.0")
  (name "hypatia")
  (type "automation-engine")
  (purpose "Neurosymbolic CI/CD intelligence with safety triangle pipeline for cross-repo security remediation")

  (position-in-ecosystem
    (category "devops-automation")
    (subcategory "ci-cd-security")
    (unique-value
      ("Safety triangle: eliminate → substitute → control")
      ("5 neural networks: Graph of Trust, MoE, LSM, ESN, RBF")
      ("Idris2 ABI with dependent type proofs + Zig C ABI bridge")
      ("Fuzzy recipe matching with language inference")
      ("Confidence-gated auto-execution (>=0.95 threshold)")
      ("Automatic feedback loop: LearningScheduler polls every 5 min")
      ("Self-diagnostics with circuit breaker and auto-recovery")
      ("Pan-forge support: GitHub, GitLab, Bitbucket, Codeberg")))

  (related-projects
    ;; Pipeline: data flow
    (project "panic-attacker"
      (relationship "data-source")
      (description "Static analysis scanner producing weak point findings")
      (integration "panic-attack xray scans repos, hypatia analyzes results"))

    (project "verisimdb-data"
      (relationship "data-store")
      (description "Git-backed flat-file store for scans, patterns, recipes, outcomes")
      (integration "Hypatia reads scans/recipes, writes patterns/outcomes/manifests"))

    (project "gitbot-fleet"
      (relationship "execution-layer")
      (description "Bot fleet for repository quality enforcement")
      (integration "Fleet coordinator executes dispatch manifests via fix scripts"))

    (project "robot-repo-automaton"
      (relationship "executor")
      (description "Automated fix execution with confidence thresholds")
      (integration "Executes auto_execute tier fixes from dispatch manifest"))

    (project "proven"
      (relationship "safety-library")
      (description "85+ formally verified safety modules (Idris2 ABI + Zig FFI)")
      (integration "Substitute-tier recipes suggest proven module replacements"))

    ;; Fleet bots
    (project "rhodibot"
      (relationship "fleet-bot")
      (description "Repository health dashboard and PR creation")
      (integration "Processes substitute-tier findings, creates review issues"))

    (project "echidnabot"
      (relationship "fleet-bot")
      (description "Verification and proof obligation tracking")
      (integration "Receives proof obligations for substitute-tier fixes"))

    (project "sustainabot"
      (relationship "fleet-bot")
      (description "Sustainability and eco-score tracking")
      (integration "Receives control-tier advisory reports"))

    ;; Infrastructure
    (project "ambientops"
      (relationship "parent")
      (description "Umbrella project for ambient computing operations")
      (integration "Hypatia is the CI/CD intelligence layer of ambientops"))

    (project "echidna"
      (relationship "verification-engine")
      (description "Formal verification and proof management")
      (integration "Validates that substitute fixes preserve behavior"))

    ;; Query Language
    (project "verisimdb"
      (relationship "query-engine")
      (description "Multi-modal database with VQL query language and hexad storage")
      (integration "ArangoDB federated alongside verisimdb-data; VQL integration planned for verisimdb_connector")
      (gap "Hypatia currently bypasses VQL, reading JSON directly — should use VQL for multi-modal queries"))

    (project "gql-dt"
      (relationship "type-system")
      (description "Lean 4 dependent type system for query verification (production-ready)")
      (integration "GQL-DT types should validate VQL queries before execution")
      (gap "Completely isolated — no consumer application uses GQL-DT types"))

    ;; Database
    (project "arangodb"
      (relationship "graph-database")
      (description "Graph query layer for trust traversal, neural state, confidence history")
      (integration "Federated alongside verisimdb-data — ArangoDB for graph queries, verisimdb for canonical store"))

    ;; External
    (project "OpenSSF Scorecard"
      (relationship "external-standard")
      (description "Security scoring for open source")
      (integration "Hypatia enforces Scorecard compliance")))

  (what-this-is
    ("Safety triangle pipeline: eliminate → substitute → control")
    ("Recipe-based automated fix system with confidence tracking")
    ("Cross-repo pattern detection and deduplication")
    ("Feedback loop: fix outcomes update recipe confidence")
    ("Fleet coordination layer for bot dispatch")
    ("Neurosymbolic learning system"))

  (what-this-is-not
    ("Not a CI/CD platform replacement - works alongside GitHub Actions")
    ("Not a monitoring tool - git-hud handles monitoring")
    ("Not a deployment pipeline - focuses on security remediation")
    ("Not a code scanner - panic-attacker handles scanning")))
