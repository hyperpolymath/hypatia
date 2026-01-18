;; SPDX-License-Identifier: PLMP-1.0-or-later
;; META.scm - Architectural decisions and development practices for cicd-hyper-a

(meta
  (version "1.0")
  (media-type "application/meta+scheme")

  (architecture-decisions
    (adr-001
      (title "Neurosymbolic Architecture")
      (status "accepted")
      (date "2024-06-01")
      (context "Need a system that can learn from CI/CD failures but execute rules with sub-millisecond latency")
      (decision "Combine neural learning layer with symbolic Logtalk/Prolog execution layer")
      (consequences
        "Enables pattern learning while maintaining deterministic rule execution"
        "Requires two separate subsystems with different deployment requirements"
        "Rules can be formally verified before deployment"))

    (adr-002
      (title "Haskell for Verification Layer")
      (status "accepted")
      (date "2024-06-01")
      (context "Need strong guarantees that rules are correct before deployment to production repositories")
      (decision "Use Haskell with Liquid Haskell for type-safe DSL and formal verification")
      (consequences
        "Property-based testing via QuickCheck catches edge cases"
        "Liquid Haskell provides refinement types for formal proofs"
        "Learning curve for contributors not familiar with Haskell"
        "Build times are longer but correctness is guaranteed"))

    (adr-003
      (title "Logtalk over Pure Prolog")
      (status "accepted")
      (date "2024-06-01")
      (context "Need logical rule execution with good modularity and maintainability")
      (decision "Use Logtalk as object-oriented extension to Prolog for rule engine")
      (consequences
        "Better code organization via Logtalk objects"
        "Can use multiple Prolog backends (SWI, GNU, SICStus)"
        "Rules are declarative and easy to audit"
        "Execution is deterministic and fast"))

    (adr-004
      (title "ArangoDB for Graph Storage")
      (status "accepted")
      (date "2024-06-01")
      (context "Need to model relationships between repos, rules, patterns, and rulesets")
      (decision "Use ArangoDB multi-model database with graph capabilities")
      (consequences
        "Native graph queries via AQL"
        "Can model complex repo-rule-pattern relationships"
        "Supports document and graph in same system"
        "Operational complexity of running ArangoDB cluster"))

    (adr-005
      (title "Dragonfly for Caching")
      (status "accepted")
      (date "2024-06-15")
      (context "Rules must execute with sub-millisecond latency at hook time")
      (decision "Use Dragonfly as Redis-compatible distributed cache for compiled rules")
      (consequences
        "Compiled rule bytecode cached for instant execution"
        "Redis protocol compatibility eases integration"
        "Multi-threaded performance better than Redis"
        "Additional infrastructure component to manage"))

    (adr-006
      (title "Rust for Forge Adapters")
      (status "accepted")
      (date "2024-07-01")
      (context "Need reliable, performant adapters for multiple Git forges")
      (decision "Write forge adapters (GitHub, GitLab, Bitbucket, Radicle) in Rust")
      (consequences
        "Memory safety and performance for long-running adapter processes"
        "Consistent with RSR language policy"
        "Strong async ecosystem (tokio) for webhook handling"
        "Can compile to single binary for easy deployment"))

    (adr-007
      (title "Git-Native Ruleset Registry")
      (status "accepted")
      (date "2024-08-01")
      (context "Need versioned, auditable storage for verified rulesets")
      (decision "Store rulesets in Git with Haskell CLI for deposit/withdraw operations")
      (consequences
        "Full version history via Git"
        "Signatures via Git commit signing"
        "Can federate via Radicle P2P"
        "Audit trail is immutable")))

  (development-practices
    (code-style
      (haskell "HLint, Ormolu formatting, -Wall -Werror")
      (logtalk "Logtalk linter, consistent predicate naming")
      (rust "Clippy strict, rustfmt, no unsafe without justification"))

    (security
      (rules "All rules property-tested before deployment")
      (registry "GPG-signed commits required for ruleset deposits")
      (adapters "OAuth tokens never logged, secrets via env vars only"))

    (testing
      (unit "QuickCheck properties for Haskell, Logtalk unit tests")
      (integration "Docker-compose test environment with all services")
      (fuzzing "Cargo-fuzz for Rust adapters"))

    (versioning
      (scheme "SemVer 2.0")
      (rulesets "Immutable once deposited, new versions create new entries")
      (api "Versioned endpoints with deprecation notices"))

    (documentation
      (format "AsciiDoc for all documentation")
      (api "OpenAPI 3.0 spec for REST endpoints")
      (rules "Each rule must have docstring explaining purpose and effects"))

    (branching
      (model "trunk-based development with short-lived feature branches")
      (protection "main branch protected, requires passing CI and review")))

  (design-rationale
    (why-neurosymbolic
      "Pure ML systems are unpredictable and slow at inference time"
      "Pure symbolic systems cannot learn new patterns automatically"
      "Hybrid approach: ML learns patterns, symbolic layer executes rules deterministically"
      "Rules can be audited and formally verified unlike neural networks")

    (why-haskell-verification
      "Type system catches entire classes of bugs at compile time"
      "Liquid Haskell adds refinement types for even stronger guarantees"
      "QuickCheck generates edge cases humans would never think of"
      "If it compiles and passes properties, high confidence it is correct")

    (why-separate-learning-and-execution
      "Learning can be expensive and run async in batch jobs"
      "Execution must be instant at git hook time"
      "Separation allows rules to be cached and verified independently"
      "Can update learning layer without changing execution layer")

    (why-multi-forge
      "Hyperpolymath mirrors to GitHub, GitLab, Bitbucket"
      "Radicle enables decentralized ruleset federation"
      "Rules should work regardless of where code is hosted"
      "Vendor lock-in to single forge is architectural risk")

    (why-dumb-rules-from-smart-learning
      "Simple rules are fast to execute"
      "Simple rules are easy to audit and understand"
      "Simple rules are less likely to have bugs"
      "Complexity is in the learning, not the execution"
      "A rule that says 'if file missing, add file' is trivially correct")))
