;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Meta-level information for hypatia
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
        ("Note: Distillation operational with 10 recipes, 6 at 0.99 confidence")))

    (adr-003
      (status "accepted")
      (date "2026-01-04")
      (title "Hook wave propagation via .git-private-farm")
      (context "Need to deploy fixes across 200+ repos without manual intervention")
      (decision "Use .git-private-farm as central hook distribution point, hypatia generates hooks")
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
        ("Con: Manual maintenance of error catalog")))

    (adr-005
      (status "accepted")
      (date "2026-02-12")
      (title "Safety triangle pipeline: eliminate > substitute > control")
      (context "Need hierarchical remediation strategy that prioritizes removing hazards over adding controls. 3260 weak points across 292 repos need systematic handling.")
      (decision "Implement 3-tier filtering: (1) Eliminate — remove hazard entirely via recipe, (2) Substitute — replace with proven-safe module, (3) Control — add guards/documentation. Use confidence thresholds for dispatch: >=0.95 auto-execute, >=0.85 review, <0.85 report-only.")
      (consequences
        ("Pro: 86.3% weak point reduction (3260 → 447)")
        ("Pro: Confidence feedback loop improves over time")
        ("Pro: Fuzzy recipe matching bridges fingerprinted IDs to clean recipe IDs")
        ("Pro: Language inference detects finding-level language from descriptions")
        ("Con: Substitute-tier requires human review for proven module integration")))

    (adr-006
      (status "accepted")
      (date "2026-02-12")
      (title "Dispatch manifest as execution bridge")
      (context "Need bridge between Elixir decision layer and bash/rust execution layer")
      (decision "Write JSONL manifest files with one entry per (pattern, repo) action. dispatch-runner.sh reads manifests and executes fix scripts or robot-repo-automaton.")
      (consequences
        ("Pro: Decouples decision from execution")
        ("Pro: Manifests are inspectable, archiveable, replayable")
        ("Pro: Works with existing fleet fix scripts")
        ("Con: Per-line jq parsing is slow for large manifests")))

    (adr-007
      (status "accepted")
      (date "2026-02-13")
      (title "Five neural network architecture for multi-signal intelligence")
      (context "Need diverse intelligence for trust routing, domain expertise, temporal patterns, trajectory forecasting, and novelty detection. Single model cannot cover all aspects.")
      (decision "Implement 5 complementary networks: (1) Graph of Trust — PageRank for entity trust scoring, (2) Mixture of Experts — domain-specific confidence with sparse gating, (3) Liquid State Machine — reservoir computing for temporal anomaly detection, (4) Echo State Network — time-series forecasting for confidence trajectories, (5) Radial Neural Network — RBF for similarity and novelty detection. Coordinated by a GenServer in the OTP supervision tree.")
      (consequences
        ("Pro: Each network specializes in a different signal type")
        ("Pro: Sparse MoE activation is efficient (top-k=2)")
        ("Pro: Reservoir networks need no backpropagation")
        ("Pro: RBF provides natural novelty detection via distance to centers")
        ("Pro: Aggregated confidence more robust than single-model estimate")
        ("Con: 5 networks have higher memory footprint than single model")
        ("Con: Reservoir networks need sufficient data to be useful")))

    (adr-008
      (status "accepted")
      (date "2026-02-13")
      (title "Idris2 ABI + Zig FFI for formal interface contracts")
      (context "Need provably correct API interface definitions that can be consumed from any language via C ABI.")
      (decision "Define all API types and operations in Idris2 with dependent type proofs (src/abi/*.idr). Implement stable C ABI in Zig (ffi/zig/src/main.zig). Cover REST, GraphQL, and gRPC interfaces with proofs that all operations return ApiResponse-wrapped types.")
      (consequences
        ("Pro: Dependent types prove interface correctness at compile time")
        ("Pro: Zig C ABI compatible with all major languages")
        ("Pro: Triple API support: REST, GraphQL, gRPC")
        ("Pro: ApiResponse wrapper proof ensures uniform error handling")
        ("Con: Requires Idris2 toolchain for ABI verification"))))

  (development-practices
    (code-style
      (elixir "mix format")
      (logtalk "Follow Logtalk coding guidelines")
      (rust "rustfmt, clippy")
      (shell "ShellCheck, POSIX-compatible"))
    (security
      (principle "Defense in depth via safety triangle")
      (sha-pinning "All GitHub Actions must be SHA-pinned")
      (permissions "Workflows use permissions: read-all by default")
      (spdx "All files have SPDX-License-Identifier header")
      (secrets "Never hardcode - use environment variables")
      (proven-modules "Substitute unsafe patterns with proven/ safety modules"))
    (testing
      (elixir "mix test")
      (logtalk "Logtalk unit tests via lgtunit")
      (integration "GitHub Actions workflow tests")
      (pipeline "mix run -e 'Hypatia.PatternAnalyzer.analyze_all_scans()'"))
    (versioning "SemVer")
    (documentation "AsciiDoc primary, Markdown for GitHub compatibility")
    (branching "main for stable, feature/* for development"))

  (design-rationale
    (why-safety-triangle "Industrial safety hierarchy: eliminate hazard > substitute safer option > add controls")
    (why-elixir "BEAM for reliable concurrent pipeline processing, pattern matching for routing")
    (why-logtalk "Provides object-oriented layer over Prolog for modular rule organization")
    (why-fuzzy-matching "Registry pattern IDs are fingerprinted, recipes use clean IDs — need bridge")
    (why-language-inference "Repo-level language doesn't match finding-level language (rust repo with shell scripts)")
    (why-confidence-thresholds "Progressive trust: recipes earn auto-execute through successful outcomes")
    (why-neurosymbolic "Combines learning capability of neural with reliability of symbolic")))
